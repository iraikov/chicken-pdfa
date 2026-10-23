;;;
;;; PDF/A-1b documents: document information, XMP
;;; metadata, ICC output intents and the trailer /ID, built on the pdf
;;; egg's public object-model API. Embedded fonts come from the
;;; pdf-truetype egg.
;;;

(module pdfa

  ( pdf-date-string
    xmp-date-string
    md5-hex
    build-xmp-packet

    build-pdfa-document
    pdfa-document?
    pdfa-document-doc
    pdfa-document-conformance
    set-document-info!
    set-output-intent!
    finalize-document-metadata!
    info-trailer-fragment
    write-pdfa-document
    pdf-text-string

    check-pdfa-conformance
    pdfa-violation?
    pdfa-violation-kind
    pdfa-violation->string
    icc-profile-problems

    (with-pdfa-document-to-file
     build-pdfa-document pdfa-document-doc write-pdfa-document)
    )

(import scheme (scheme base) (chicken base) (chicken format) (chicken port)
        (chicken string) (chicken bitwise) (chicken bytevector)
        (chicken process-context posix) (chicken random) (chicken condition)
        srfi-1 srfi-13 message-digest md5 (chicken time) (chicken time posix)
        sxml-transforms
        datatype
        pdf pdf-deflate pdf-truetype)

(define fmt format)

;; Formats a PDF date string (the /CreationDate value) from a Unix time.
;; seconds: a Unix timestamp, as returned by current-seconds.
;; Returns a string of the form "D:YYYYMMDDHHMMSSZ".
(define (pdf-date-string seconds)
  (string-append "D:" (time->string (seconds->utc-time seconds) "%Y%m%d%H%M%SZ")))

;; Formats an XMP/ISO-8601 date string from a Unix time.
;; seconds: a Unix timestamp, as returned by current-seconds.
;; Returns a string of the form "YYYY-MM-DDTHH:MM:SSZ".
(define (xmp-date-string seconds)
  (time->string (seconds->utc-time seconds) "%Y-%m-%dT%H:%M:%SZ"))

;; Computes the MD5 digest of a string.
;; s: the string to hash.
;; Returns the digest as a 32-character lowercase hex string.
(define (md5-hex s)
  (message-digest-string (md5-primitive) s))

;; Rewrite rules for serializing an XMP packet's SXML tree to XML text
;; with sxml-transforms' pre-post-order/SRV:send-reply, adding a rule
;; for processing-instruction nodes (*PI* target attributes ...) on top
;; of the library's own element/attribute/text handling.
(define xmp-rules
  `((@ ((*default* . ,(lambda (attr-key . value) (enattr attr-key value))))
       . ,(lambda (trigger . value) (cons '@ value)))
    (*default* . ,(lambda (tag . elems) (entag tag elems)))
    (*text* . ,(lambda (trigger str) (if (string? str) (string->goodHTML str) str)))
    (*PI* . ,(lambda (trigger target . attrs)
               (list "<?" (symbol->string target)
                     (if (and (pair? attrs) (pair? (car attrs)) (eq? '@ (caar attrs)))
                         (cdar attrs)
                         '())
                     "?>")))))

;; Serializes an SXML tree to an XML text string.
;; sxml: a list of SXML nodes.
;; Returns the serialized XML as a string.
(define (sxml-to-xml sxml)
  (with-output-to-string (lambda () (SRV:send-reply (pre-post-order sxml xmp-rules)))))

;; Builds an XMP field holding a single language-alternative value.
;; tag: the XMP property name, as a symbol.
;; value: the field's text, or #f/empty to omit the field.
;; Returns the field as an SXML node, or '() when value is absent.
(define (xmp-field-alt tag value)
  (if (and value (> (string-length value) 0))
      `(,tag (rdf:Alt (rdf:li (@ (xml:lang "x-default")) ,value)))
      '()))

;; Builds an XMP field holding a single-item ordered sequence.
;; tag: the XMP property name, as a symbol.
;; value: the field's text, or #f/empty to omit the field.
;; Returns the field as an SXML node, or '() when value is absent.
(define (xmp-field-seq tag value)
  (if (and value (> (string-length value) 0))
      `(,tag (rdf:Seq (rdf:li ,value)))
      '()))

;; Builds an XMP field holding a plain text value.
;; tag: the XMP property name, as a symbol.
;; value: the field's text, or #f/empty to omit the field.
;; Returns the field as an SXML node, or '() when value is absent.
(define (xmp-field-simple tag value)
  (if (and value (> (string-length value) 0))
      `(,tag ,value)
      '()))

;; Builds a complete XMP metadata packet describing a PDF document.
;; title, author, subject, keywords, producer, creator-tool: document
;; properties as strings; each is omitted from the packet when absent.
;; create-date: the document's creation date, as an XMP date string.
;; pdfa-part, pdfa-conformance: the PDF/A identification fields, as
;; strings (for PDF/A-1b, "1" and "B"). When both are given, the packet
;; declares the PDF/A identification schema and records them; this is
;; the file's claim to conform, so callers pass them only after the
;; document has been checked.
;; Returns the packet as a plain Unicode string (not UTF-8-encoded),
;; wrapped in the <?xpacket ...?> processing instructions.
(define (build-xmp-packet #!key title author subject keywords producer creator-tool create-date
                          pdfa-part pdfa-conformance)
  (let ((claim? (and pdfa-part pdfa-conformance)))
    (sxml-to-xml
     (list
      `(*PI* xpacket (@ (begin "") (id "W5M0MpCehiHzreSzNTczkc9d")))
      `(x:xmpmeta (@ (xmlns:x "adobe:ns:meta/"))
         (rdf:RDF (@ (xmlns:rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#"))
           (rdf:Description
            (@ (rdf:about "")
               (xmlns:dc "http://purl.org/dc/elements/1.1/")
               (xmlns:xmp "http://ns.adobe.com/xap/1.0/")
               (xmlns:pdf "http://ns.adobe.com/pdf/1.3/")
               ,@(if claim? '((xmlns:pdfaid "http://www.aiim.org/pdfa/ns/id/")) '()))
            (dc:format "application/pdf")
            ,(xmp-field-alt 'dc:title title)
            ,(xmp-field-seq 'dc:creator author)
            ,(xmp-field-alt 'dc:description subject)
            ,(xmp-field-simple 'pdf:Keywords keywords)
            ,(xmp-field-simple 'pdf:Producer producer)
            ,(xmp-field-simple 'xmp:CreatorTool creator-tool)
            ,(xmp-field-simple 'xmp:CreateDate create-date)
            ,(if claim? (xmp-field-simple 'pdfaid:part pdfa-part) '())
            ,(if claim? (xmp-field-simple 'pdfaid:conformance pdfa-conformance) '()))))
      `(*PI* xpacket (@ (end "w")))))))

;; ---------------------------------------------------------------------
;; Document information dictionary, XMP metadata stream, ICC output
;; intent, and trailer /ID.

;; Name recorded as this library's /Producer and the XMP pdf:Producer field.
(define *pdf-producer* "chicken-pdfa")

;; pdfa-document: wraps a plain pdf `doc` together with the PDF/A-1b
;; metadata for that document. Create one with build-pdfa-document and
;; pass it explicitly to every procedure that needs it -- there is no
;; shared global document, so independent documents never interfere.
(define-record pdfa-document
  doc               ; the wrapped pdf `doc` record (PDF version 1.4)
  conformance       ; 'pdfa-1b to check and claim PDF/A-1b, or #f for metadata only
  document-info     ; alist of Info fields from set-document-info!, or #f
  document-info-obj ; indirect-obj for /Info, once finalize-document-metadata! builds it
  output-intent     ; (icc-bytevector n condition-identifier info) list, or #f
  document-id-hex)  ; trailer /ID hex digest, once computed

;; Constructs a fresh PDF/A document, wrapping a fresh PDF 1.4 pdf `doc`.
;; compress-streams? sets the wrapped document's default stream
;; compression behavior (the XMP metadata stream itself is always stored
;; uncompressed regardless, since PDF/A forbids filtering it).
;; conformance is 'pdfa-1b (the default) to check the finished document
;; against the PDF/A-1b rules and record the claim in its metadata, or #f
;; to write the same metadata with no checks and no claim.
(define (build-pdfa-document #!optional (compress-streams? #t) (conformance 'pdfa-1b))
  (unless (memq conformance '(pdfa-1b #f))
    (error 'build-pdfa-document "conformance must be 'pdfa-1b or #f" conformance))
  (make-pdfa-document (build-doc compress-streams? "1.4") conformance #f #f #f #f))

;; Looks up a field in a document-info alist.
;; info: an alist as produced by set-document-info!, or #f.
;; key: the field name to look up, as a string.
;; Returns the field's value, or #f if absent.
(define (info-ref info key)
  (and info (let ((p (assoc key info))) (and p (cdr p)))))

;; Records the document information fields for pdfa-doc.
;; title, author, subject, keywords: descriptive document properties.
;; creator: the name of the application that produced the document's
;; content, distinct from the producer (this library).
;;
;; Fields given as #f or as the empty string are left out, so that the
;; /Info dictionary and the XMP packet always list the same fields.
;; A value containing a control character other than tab, line feed or
;; carriage return is an error: XML 1.0 cannot represent it, so the XMP
;; copy could not match the /Info copy.
(define (set-document-info! pdfa-doc #!key title author subject keywords creator)
  (let ((fields (filter (lambda (kv) (and (cdr kv) (not (equal? (cdr kv) ""))))
                        (list (cons "Title" title)
                              (cons "Author" author)
                              (cons "Subject" subject)
                              (cons "Keywords" keywords)
                              (cons "Creator" creator)))))
    (for-each
     (lambda (kv)
       (unless (string? (cdr kv))
         (error 'set-document-info! "document information values must be strings" (car kv) (cdr kv)))
       (when (string-any xml-forbidden-char? (cdr kv))
         (error 'set-document-info! "control character not allowed in document information" (car kv))))
     fields)
    (pdfa-document-document-info-set! pdfa-doc fields)))

;; True for the control characters XML 1.0 does not allow in text: all
;; below U+0020 except tab, line feed and carriage return.
(define (xml-forbidden-char? c)
  (and (char<? c #\space) (not (memv c '(#\tab #\newline #\return)))))

;; Loads an ICC profile and records it as pdfa-doc's output intent.
;; icc-path: filesystem path to the ICC profile to embed.
;; n: the profile's number of color components.
;; condition-identifier: the standard output condition name (e.g. "sRGB").
;; info: a human-readable description of the output condition.
;;
;; The profile's bytes are kept as a bytevector and passed to the stream
;; writer as bytevector content.
;;
;; In conformance mode the profile's header is checked at once (see
;; icc-profile-problems), so a profile PDF/A-1 cannot use fails here
;; rather than when the document is written.
(define (set-output-intent! pdfa-doc icc-path #!key
                            (n 3) (condition-identifier "sRGB") (info "sRGB IEC61966-2.1"))
  (let* ((bv (read-byte-file icc-path))
         (problems (icc-profile-problems bv n)))
    (when (and (pdfa-document-conformance pdfa-doc) (pair? problems))
      (error 'set-output-intent! (string-append "ICC profile cannot be used for PDF/A-1: "
                                                (string-intersperse problems "; "))
             icc-path))
    (pdfa-document-output-intent-set!
     pdfa-doc
     (list bv n condition-identifier info))))

;; Returns the four-character signature at offset i of an ICC profile,
;; or #f if the profile is too short.
(define (icc-signature bv i)
  (and (<= (+ i 4) (bytevector-length bv))
       (list->string (map (lambda (k) (integer->char (bytevector-u8-ref bv (+ i k)))) '(0 1 2 3)))))

;; The number of color components of each ICC color space PDF/A-1
;; output intents use.
(define +icc-color-spaces+ '(("RGB " . 3) ("CMYK" . 4) ("GRAY" . 1)))

;; Checks an ICC profile's header for use as a PDF/A-1 output intent
;; with n color components. Returns a list of problem descriptions,
;; empty if there are none: the declared size must match the data, the
;; device class must be a monitor or printer profile, the color space
;; must be RGB, CMYK or gray with n matching it, and the major version
;; must be below 4, since PDF 1.4 predates version 4 profiles.
(define (icc-profile-problems bv n)
  (let ((len (bytevector-length bv)))
    (if (< len 128)
        (list "file is too short to be an ICC profile")
        (let* ((size (+ (* 16777216 (bytevector-u8-ref bv 0)) (* 65536 (bytevector-u8-ref bv 1))
                        (* 256 (bytevector-u8-ref bv 2)) (bytevector-u8-ref bv 3)))
               (major (bytevector-u8-ref bv 8))
               (class (icc-signature bv 12))
               (space (icc-signature bv 16))
               (space-n (let ((e (assoc space +icc-color-spaces+))) (and e (cdr e)))))
          (filter identity
                  (list (and (not (= size len))
                             (fmt #f "header size ~a does not match file length ~a" size len))
                        (and (not (member class '("mntr" "prtr")))
                             (fmt #f "device class ~s is not a monitor or printer profile" class))
                        (and (not space-n)
                             (fmt #f "color space ~s is not RGB, CMYK or gray" space))
                        (and space-n (not (eqv? n space-n))
                             (fmt #f "/N is ~a but the ~a profile has ~a components" n (string-trim-right space) space-n))
                        (and (>= major 4)
                             (fmt #f "profile version ~a is too new; PDF/A-1 needs version 2 or 3" major))))))))

;; Returns the output intent's color space as one of the strings "RGB",
;; "CMYK" or "GRAY", or #f when there is no usable output intent.
(define (output-intent-color-space pdfa-doc)
  (let ((oi (pdfa-document-output-intent pdfa-doc)))
    (and oi
         (let ((space (icc-signature (car oi) 16)))
           (and space (assoc space +icc-color-spaces+) (string-trim-right space))))))

;; Wraps a string as a PDF literal string, escaping parentheses and backslashes.
;; s: the text to wrap.
;; Returns the text enclosed in parentheses, e.g. "(hello)".
(define (pdf-literal-string s)
  (string-append "(" (escape s) ")"))

;; Encodes a string as a PDF text string (PDF 1.4 section 3.8.1), for
;; document information values. Text made only of printable ASCII
;; characters becomes an escaped literal string, since ASCII reads the
;; same in PDFDocEncoding. Any other text becomes a hex string holding a
;; byte-order mark and the UTF-16BE code units of each character, with
;; characters above U+FFFF written as surrogate pairs. Hex strings are
;; plain ASCII, so the output port's character encoding cannot alter
;; them.
(define (pdf-text-string s)
  (if (string-every (lambda (c) (char<=? #\space c #\~)) s)
      (pdf-literal-string s)
      (string-append "<FEFF" (string-concatenate (map utf16be-hex (string->list s))) ">")))

;; Returns the UTF-16BE code units of one character as uppercase hex
;; digits: four digits, or eight for a surrogate pair.
(define (utf16be-hex c)
  (let ((n (char->integer c)))
    (if (< n #x10000)
        (hex4 n)
        (let ((m (- n #x10000)))
          (string-append (hex4 (+ #xD800 (arithmetic-shift m -10)))
                         (hex4 (+ #xDC00 (bitwise-and m #x3FF))))))))

;; Formats an integer 0..#xFFFF as four uppercase hex digits.
(define (hex4 n)
  (string-pad (string-upcase (number->string n 16)) 4 #\0))

;; Builds the document information dictionary (/Info).
;; info: an alist of field name/value pairs, as produced by set-document-info!.
;; now: the document's creation time, as a Unix timestamp.
;; Returns a pdf dictionary object with the caller's fields plus
;; /Producer and /CreationDate.
(define (build-info-dictionary info now)
  (build-dictionary
   (append
    (map (lambda (kv) (cons (string-append "/" (car kv)) (pdf-text-string (cdr kv)))) info)
    (list (cons "/Producer" (pdf-literal-string *pdf-producer*))
          (cons "/CreationDate" (pdf-literal-string (pdf-date-string now)))))))

;; Builds the XMP metadata stream object (/Metadata).
;; doc: the pdf document to build the stream's indirect object in.
;; info: an alist of document information fields, or #f.
;; now: the document's creation time, as a Unix timestamp.
;; claim?: true to record the PDF/A-1b identification fields.
;; Returns an indirect object wrapping the UTF-8-encoded XMP packet,
;; always stored uncompressed as PDF/A requires.
(define (build-metadata-stream-obj doc info now claim?)
  (let* ((xmp-text (build-xmp-packet
                     title: (info-ref info "Title")
                     author: (info-ref info "Author")
                     subject: (info-ref info "Subject")
                     keywords: (info-ref info "Keywords")
                     producer: *pdf-producer*
                     creator-tool: (info-ref info "Creator")
                     create-date: (xmp-date-string now)
                     pdfa-part: (and claim? "1")
                     pdfa-conformance: (and claim? "B")))
         (xmp-bytes (string->utf8 xmp-text))
         (stream (build-pdf-stream xmp-bytes
                                    '(("/Type" . "/Metadata") ("/Subtype" . "/XML"))
                                    #t)))
    (build-indirect-obj doc stream)))

;; Builds the output intent dictionary for the /OutputIntents array.
;; doc: the pdf document to build the ICC profile's indirect object in.
;; output-intent: the (icc-byte-string n condition-identifier info) list
;; recorded by set-output-intent!.
;; Returns a pdf dictionary object identifying this as a PDF/A-flavored
;; output intent (/S /GTS_PDFA1), with the profile embedded as an
;; indirect stream object.
(define (build-output-intent-dict doc output-intent)
  (let* ((icc-bytes (car output-intent)) (n (cadr output-intent))
         (condition-identifier (caddr output-intent)) (info-str (cadddr output-intent))
         (icc-stream (build-pdf-stream icc-bytes (list (cons "/N" n))))
         (icc-obj (build-indirect-obj doc icc-stream)))
    (build-dictionary
     (list (cons "/Type" "/OutputIntent")
           (cons "/S" "/GTS_PDFA1")
           (cons "/OutputConditionIdentifier" (pdf-literal-string condition-identifier))
           (cons "/Info" (pdf-literal-string info-str))
           (cons "/DestOutputProfile" (get-obj-ref icc-obj))))))

;; Builds the trailer dictionary fragment carrying /Info and /ID for pdfa-doc.
;; Returns a string to append after /Root in the trailer dictionary;
;; empty for whichever of /Info or /ID has not yet been set.
(define (info-trailer-fragment pdfa-doc)
  (string-append
   (if (pdfa-document-document-info-obj pdfa-doc)
       (string-append " /Info " (get-obj-ref (pdfa-document-document-info-obj pdfa-doc)))
       "")
   (if (pdfa-document-document-id-hex pdfa-doc)
       (fmt #f " /ID [ <~a> <~a> ]" (pdfa-document-document-id-hex pdfa-doc) (pdfa-document-document-id-hex pdfa-doc))
       "")))

;; Builds and wires in the document information dictionary, XMP metadata
;; stream, output intent, and trailer /ID hash for pdfa-doc. Must be
;; called once, after all pages and content have been added, before the
;; document is written. In conformance mode it first runs
;; check-pdfa-conformance and raises an error listing every violation,
;; before anything is added to the document; the PDF/A claim in the XMP
;; packet is therefore only ever written for a document that passed.
(define (finalize-document-metadata! pdfa-doc)
  (when (pdfa-document-conformance pdfa-doc)
    (let ((violations (check-pdfa-conformance pdfa-doc)))
      (unless (null? violations)
        (error 'write-pdfa-document
               (string-append "document does not conform to PDF/A-1b:\n  "
                              (string-intersperse (map pdfa-violation->string violations) "\n  "))))))
  (let* ((doc (pdfa-document-doc pdfa-doc))
         (info (pdfa-document-document-info pdfa-doc))
         (now (current-seconds))
         (catalog-dict (indirect-obj-content (doc-catalog doc))))
    (when info
      (pdfa-document-document-info-obj-set! pdfa-doc (build-indirect-obj doc (build-info-dictionary info now))))
    (let ((metadata-obj (build-metadata-stream-obj doc info now
                                                   (pdfa-document-conformance pdfa-doc))))
      (add-dictionary-item catalog-dict "/Metadata" (get-obj-ref metadata-obj)))
    (when (pdfa-document-output-intent pdfa-doc)
      (add-dictionary-item catalog-dict "/OutputIntents"
                            (vector (build-output-intent-dict doc (pdfa-document-output-intent pdfa-doc)))))
    ;; The hash only needs to distinguish this document from others, not
    ;; be cryptographically meaningful, so a random value mixed in with
    ;; the process id and current time is enough -- no persistent counter
    ;; needed.
    (pdfa-document-document-id-hex-set!
     pdfa-doc
     (md5-hex (fmt #f "~a:~a:~a:~a" *pdf-producer* now (current-process-id) (pseudo-random-integer 1000000000))))))

;; ---------------------------------------------------------------------
;; PDF/A-1b conformance checks, run on the finished document before it
;; is written. Each problem found is a pdfa-violation; clause numbers in
;; the messages refer to ISO 19005-1:2005.

(define (any-value? x) #t)

(define-datatype pdfa-violation pdfa-violation?
  (missing-output-intent)
  (icc-profile-invalid (reason string?))
  (unembedded-font (resource-name string?) (base-font string?))
  (restricted-font-license (base-font string?))
  (color-space-mismatch (operator string?) (intent-space string?))
  (transparency (resource-name string?) (key string?) (value any-value?))
  (limit-exceeded (what string?) (value any-value?)))

;; Returns the kind of a violation, as the symbol naming its variant.
(define (pdfa-violation-kind v)
  (cases pdfa-violation v
    (missing-output-intent () 'missing-output-intent)
    (icc-profile-invalid (reason) 'icc-profile-invalid)
    (unembedded-font (resource-name base-font) 'unembedded-font)
    (restricted-font-license (base-font) 'restricted-font-license)
    (color-space-mismatch (operator intent-space) 'color-space-mismatch)
    (transparency (resource-name key value) 'transparency)
    (limit-exceeded (what value) 'limit-exceeded)))

;; Describes a violation in one line of plain text.
(define (pdfa-violation->string v)
  (cases pdfa-violation v
    (missing-output-intent ()
      "no output intent: call set-output-intent! with an ICC profile (6.2.2)")
    (icc-profile-invalid (reason)
      (fmt #f "the output intent's ICC profile cannot be used: ~a (6.2.2)" reason))
    (unembedded-font (resource-name base-font)
      (fmt #f "font ~a (~a) is not embedded; use build-embedded-font from pdf-truetype (6.3.4)"
           resource-name base-font))
    (restricted-font-license (base-font)
      (fmt #f "the licence of font ~a (OS/2 fsType) does not allow embedding (6.3.4)" base-font))
    (color-space-mismatch (operator intent-space)
      (if (string=? intent-space "missing")
          (fmt #f "content stream operator ~a sets a device color, which needs an output intent (6.2.3.3)"
               operator)
          (fmt #f "content stream operator ~a sets a device color space other than the ~a output intent's (6.2.3.3)"
               operator intent-space)))
    (transparency (resource-name key value)
      (fmt #f "graphics state ~a sets ~a to ~a; PDF/A-1 forbids transparency (6.4)"
           resource-name key value))
    (limit-exceeded (what value)
      (fmt #f "~a exceeds a PDF/A-1 implementation limit: ~a (6.1.12)" what value))))

;; Follows a value the pdf egg stores lazily (a procedure computing it
;; at write time) to the value itself.
(define (force-value v)
  (if (procedure? v) (force-value (v)) v))

;; Finds the indirect object a reference string such as "5 0 R" points
;; to among doc's objects, or #f.
(define (resolve-ref doc ref)
  (let ((ref (force-value ref)))
    (and (string? ref)
         (find (lambda (o) (equal? (get-obj-ref o) ref)) (doc-objects doc)))))

;; Looks up key in an indirect object's dictionary content, forcing
;; lazy values; #f when the object is not a dictionary or lacks the key.
(define (obj-dict-ref obj key)
  (let ((d (and obj (indirect-obj-content obj))))
    (and (dictionary? d) (force-value (get-dictionary-value d key)))))

;; The PDF/A-1 implementation limits checked in content streams (Table
;; 1 of ISO 19005-1, via PDF Reference Appendix C).
(define +max-real+ 32767)
(define +max-integer+ 2147483647)
(define +max-string-length+ 65535)
(define +max-name-length+ 127)
(define +max-q-nesting+ 28)

;; Splits a content stream into tokens, calling (on-operator name
;; operands) for each operator with the numbers given before it, and
;; (on-string length) for each literal or hex string. Literal strings
;; are skipped as a whole, honouring backslash escapes and nested
;; parentheses, so text inside them is never read as an operator.
(define (scan-content-stream s on-operator on-string)
  (let ((n (string-length s)))
    (define (delimiter? c) (memv c '(#\( #\) #\< #\> #\[ #\] #\{ #\} #\/ #\%)))
    (define (regular? c) (not (or (char-whitespace? c) (delimiter? c))))
    (define (token-end i)
      (if (and (< i n) (regular? (string-ref s i))) (token-end (+ i 1)) i))
    (let loop ((i 0) (operands '()))
      (if (>= i n)
          #t
          (let ((c (string-ref s i)))
            (cond ((char-whitespace? c) (loop (+ i 1) operands))
                  ((char=? c #\%)
                   (let skip ((j i))
                     (if (or (>= j n) (memv (string-ref s j) '(#\newline #\return)))
                         (loop j operands)
                         (skip (+ j 1)))))
                  ((char=? c #\()
                   (let str ((j (+ i 1)) (depth 1) (len 0))
                     (cond ((>= j n) (on-string len) (loop j '()))
                           ((char=? (string-ref s j) #\\) (str (+ j 2) depth (+ len 1)))
                           ((char=? (string-ref s j) #\() (str (+ j 1) (+ depth 1) (+ len 1)))
                           ((char=? (string-ref s j) #\))
                            (if (= depth 1)
                                (begin (on-string len) (loop (+ j 1) operands))
                                (str (+ j 1) (- depth 1) (+ len 1))))
                           (else (str (+ j 1) depth (+ len 1))))))
                  ((char=? c #\<)
                   (let hex ((j (+ i 1)) (digits 0))
                     (cond ((>= j n) (loop j operands))
                           ((char=? (string-ref s j) #\>)
                            (on-string (quotient (+ digits 1) 2))
                            (loop (+ j 1) operands))
                           ((char-whitespace? (string-ref s j)) (hex (+ j 1) digits))
                           (else (hex (+ j 1) (+ digits 1))))))
                  ((memv c '(#\[ #\] #\{ #\} #\> #\)))
                   (loop (+ i 1) operands))
                  ((char=? c #\/)
                   (loop (token-end (+ i 1)) operands))
                  (else
                   (let* ((j (max (token-end i) (+ i 1)))
                          (tok (substring s i j))
                          (num (string->number tok)))
                     (if num
                         (loop j (cons num operands))
                         (begin (on-operator tok (reverse operands))
                                (loop j '())))))))))))

;; Checks one page content stream, returning its violations. space is
;; the output intent's color space ("RGB", "CMYK", "GRAY"), or #f when
;; the document has none, in which case every device color operator is
;; a violation.
(define (check-content-stream content space)
  (let ((found '())
        (depth 0)
        (deepest 0))
    (define (add! v) (set! found (cons v found)))
    (define (check-number! x)
      (cond ((and (exact-integer? x) (> (abs x) +max-integer+))
             (add! (limit-exceeded "an integer" x)))
            ((and (not (exact-integer? x)) (> (abs x) +max-real+))
             (add! (limit-exceeded "a real number" x)))))
    (scan-content-stream
     content
     (lambda (op operands)
       (for-each check-number! operands)
       (cond ((string=? op "q")
              (set! depth (+ depth 1))
              (set! deepest (max deepest depth)))
             ((string=? op "Q") (set! depth (max 0 (- depth 1))))
             ;; Device color needs an output intent: DeviceRGB an RGB
             ;; one, DeviceCMYK a CMYK one, DeviceGray any.
             ((and (member op '("rg" "RG" "k" "K" "g" "G")) (not space))
              (add! (color-space-mismatch op "missing")))
             ((and (member op '("rg" "RG")) (not (string=? space "RGB")))
              (add! (color-space-mismatch op space)))
             ((and (member op '("k" "K")) (not (string=? space "CMYK")))
              (add! (color-space-mismatch op space)))))
     (lambda (len)
       (when (> len +max-string-length+)
         (add! (limit-exceeded "a string's length" len)))))
    (when (> deepest +max-q-nesting+)
      (add! (limit-exceeded "q (save graphics state) nesting" deepest)))
    (reverse found)))

;; True when a font dictionary object has an embedded font program: a
;; /FontDescriptor whose dictionary names a /FontFile, /FontFile2 or
;; /FontFile3 stream.
(define (font-embedded? doc font)
  (let ((descriptor (resolve-ref doc (obj-dict-ref font "/FontDescriptor"))))
    (and descriptor
         (any (lambda (key) (resolve-ref doc (obj-dict-ref descriptor key)))
              '("/FontFile" "/FontFile2" "/FontFile3")))))

;; True when a font dictionary object's embedded TrueType program has a
;; licence (OS/2 fsType) that forbids embedding.
(define (font-licence-restricted? doc font)
  (let* ((descriptor (resolve-ref doc (obj-dict-ref font "/FontDescriptor")))
         (file (and descriptor (resolve-ref doc (obj-dict-ref descriptor "/FontFile2"))))
         (stream (and file (indirect-obj-content file)))
         (bytes (and (pdf-stream? stream) (pdf-stream-content stream))))
    (and (bytevector? bytes)
         (handle-exceptions exn #f
           (truetype-embedding-restricted? (read-truetype-font bytes))))))

;; Checks pdfa-doc against the PDF/A-1b rules this library can break,
;; returning the list of violations found, empty when it conforms. The
;; checks cover the output intent, fonts, transparency in graphics
;; state resources, and every page content stream (device color
;; against the output intent, and the implementation limits).
(define (check-pdfa-conformance pdfa-doc)
  (let* ((doc (pdfa-document-doc pdfa-doc))
         (oi (pdfa-document-output-intent pdfa-doc))
         (space (output-intent-color-space pdfa-doc))
         (intent
          (if oi
              (map icc-profile-invalid (icc-profile-problems (car oi) (cadr oi)))
              (list (missing-output-intent))))
         (fonts
          (append-map
           (lambda (font)
             (let ((resource (indirect-obj-name font))
                   (base (or (obj-dict-ref font "/BaseFont") "unnamed")))
               (append
                (if (font-embedded? doc font)
                    (if (font-licence-restricted? doc font)
                        (list (restricted-font-license (substring base 1)))
                        '())
                    (list (unembedded-font resource base)))
                (if (> (string-length resource) (+ 1 +max-name-length+))
                    (list (limit-exceeded "a font resource name's length" (string-length resource)))
                    '()))))
           (reverse (doc-fonts doc))))
         (gstates
          (append-map
           (lambda (gs)
             (let ((name (indirect-obj-name gs)))
               (filter identity
                       (list (let ((ca (obj-dict-ref gs "/ca")))
                               (and ca (not (= ca 1)) (transparency name "/ca" ca)))
                             (let ((ca (obj-dict-ref gs "/CA")))
                               (and ca (not (= ca 1)) (transparency name "/CA" ca)))
                             (let ((bm (obj-dict-ref gs "/BM")))
                               (and bm (not (member bm '("/Normal" "/Compatible")))
                                    (transparency name "/BM" bm)))
                             (let ((sm (obj-dict-ref gs "/SMask")))
                               (and sm (not (equal? sm "/None")) (transparency name "/SMask" sm)))))))
           (reverse (doc-ext-gstates doc))))
         (content
          (append-map
           (lambda (page)
             (let* ((obj (resolve-ref doc (obj-dict-ref page "/Contents")))
                    (stream (and obj (indirect-obj-content obj)))
                    (text (and (pdf-stream? stream) (pdf-stream-content stream))))
               (if (string? text) (check-content-stream text space) '())))
           (reverse (doc-pages doc)))))
    (delete-duplicates (append intent fonts gstates content) equal?)))

;; The comment line written straight after the %PDF-1.4 header: a %
;; followed by four bytes above 127, as PDF/A-1 requires (ISO 19005-1
;; section 6.1.2), so that file-transfer tools treat the file as binary.
(define +binary-header-comment+ (bytevector 37 226 227 207 211))

;; Finalizes pdfa-doc's metadata and writes the document to filename,
;; with the PDF/A binary header comment and the /Info and /ID trailer
;; entries.
(define (write-pdfa-document pdfa-doc filename)
  (finalize-document-metadata! pdfa-doc)
  (write-document (pdfa-document-doc pdfa-doc) filename "1.4"
                  (info-trailer-fragment pdfa-doc) +binary-header-comment+))

;; Builds a PDF/A-1b document and writes it to a file. Binds both
;; pdfa-doc (for set-document-info!/set-output-intent!) and doc, the
;; wrapped plain pdf document (for ordinary pdf calls -- build-font,
;; with-page, drawing operators), so body can use whichever it needs.
;; filename: the path to write the finished PDF to.
;; body: forms that build the document's pages and content.
;; The optional binding forms (pdfa-doc doc compress-streams?) and
;; (pdfa-doc doc compress-streams? conformance) pass those settings to
;; build-pdfa-document; conformance defaults to 'pdfa-1b.
(define-syntax with-pdfa-document-to-file
  (syntax-rules ()
    ((_ (pdfa-doc doc) filename body ...)
     (with-pdfa-document-to-file (pdfa-doc doc #t 'pdfa-1b) filename body ...))
    ((_ (pdfa-doc doc compress-streams?) filename body ...)
     (with-pdfa-document-to-file (pdfa-doc doc compress-streams? 'pdfa-1b) filename body ...))
    ((_ (pdfa-doc doc compress-streams? conformance) filename body ...)
     (let* ((pdfa-doc (build-pdfa-document compress-streams? conformance))
            (doc (pdfa-document-doc pdfa-doc)))
       body ...
       (write-pdfa-document pdfa-doc filename)))))


)
