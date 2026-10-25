;;;
;;; Unit tests for the pdfa egg.
;;;

(import scheme (scheme base) (chicken base) (chicken file) (chicken file posix) (chicken io)
        (chicken bytevector) (chicken process) (chicken condition)
        pdf pdfa pdf-truetype pdf-deflate test)

;; ---------------------------------------------------------------------
;; helpers

(define (string-prefix? prefix str)
  (and (>= (string-length str) (string-length prefix))
       (string=? prefix (substring str 0 (string-length prefix)))))

(define (string-suffix? suffix str)
  (and (>= (string-length str) (string-length suffix))
       (string=? suffix (substring str (- (string-length str) (string-length suffix))
                                    (string-length str)))))

(define (count-occurrences needle haystack)
  (let ((nlen (string-length needle))
        (hlen (string-length haystack)))
    (let loop ((i 0) (n 0))
      (if (> (+ i nlen) hlen)
          n
          (loop (+ i 1)
                (if (string=? needle (substring haystack i (+ i nlen)))
                    (+ n 1)
                    n))))))

(define (read-file path)
  (call-with-input-file path
    (lambda (p)
      (let ((bv (read-bytevector (file-size path) p)))
        (bytevector->byte-string bv (bytevector-length bv))))))

;; True when the file at path contains the bytes of the ASCII string
;; needle. Searches the file's bytes directly, for files too large to
;; search as a string: in CHICKEN 6 a string holding characters above
;; 127 is stored as UTF-8, so substring-index (below) on it takes time
;; that grows with the square of its length.
(define (file-contains? path needle)
  (let* ((hay (call-with-input-file path (lambda (p) (read-bytevector (file-size path) p))))
         (hlen (bytevector-length hay))
         (pat (list->vector (map char->integer (string->list needle))))
         (nlen (vector-length pat)))
    (let loop ((i 0))
      (cond ((> (+ i nlen) hlen) #f)
            ((let match ((j 0))
               (or (= j nlen)
                   (and (= (bytevector-u8-ref hay (+ i j)) (vector-ref pat j))
                        (match (+ j 1)))))
             #t)
            (else (loop (+ i 1)))))))

;; Plain substring search; returns #f if not found.
(define (substring-index needle haystack start)
  (let ((nlen (string-length needle))
        (hlen (string-length haystack)))
    (let loop ((i start))
      (cond ((> (+ i nlen) hlen) #f)
            ((string=? needle (substring haystack i (+ i nlen))) i)
            (else (loop (+ i 1)))))))

;; Parse the run of ASCII digits starting at pos.
(define (parse-uint-at contents pos)
  (let loop ((i pos) (n 0))
    (if (and (< i (string-length contents)) (char-numeric? (string-ref contents i)))
        (loop (+ i 1) (+ (* n 10) (- (char->integer (string-ref contents i))
                                      (char->integer #\0))))
        n)))

;; Pull the raw bytes of the Nth (0-based) stream object out of a raw PDF
;; file's contents, for round-tripping through flate-decompress in
;; end-to-end tests. Driven entirely by each object's own declared
;; /Length value (as a real PDF parser would use it), rather than by
;; searching for a closing "endstream" marker -- a compressed stream's
;; binary payload can, by chance, contain the literal bytes "stream\n" or
;; "endstream" (observed in practice), which would misdirect a naive text
;; search.
(define (extract-stream-bytes contents occurrence)
  (let loop ((start 0) (n occurrence))
    (let ((length-pos (substring-index "/Length " contents start)))
      (unless length-pos (error 'extract-stream-bytes "no more streams found"))
      (let* ((digits-start (+ length-pos (string-length "/Length ")))
             (declared-length (parse-uint-at contents digits-start))
             (stream-pos (substring-index "stream\n" contents digits-start)))
        (unless stream-pos (error 'extract-stream-bytes "stream keyword not found after /Length"))
        (let ((body-start (+ stream-pos (string-length "stream\n"))))
          (if (= n 0)
              (substring contents body-start (+ body-start declared-length))
              (loop (+ body-start declared-length) (- n 1))))))))

;; Locate the start of the Nth (0-based) "/Length " marker's stream
;; dictionary text, up to (not including) the following "stream\n" --
;; write-pdf-stream always places /Filter (when present) immediately
;; after /Length, before that point, so checking this narrow span is
;; exactly equivalent to checking the whole dictionary.
(define (stream-dict-text contents occurrence)
  (let loop ((start 0) (n occurrence))
    (let ((length-pos (substring-index "/Length " contents start)))
      (unless length-pos (error 'stream-dict-text "no more streams found"))
      (let ((stream-pos (substring-index "stream\n" contents length-pos)))
        (unless stream-pos (error 'stream-dict-text "stream keyword not found after /Length"))
        (if (= n 0)
            (substring contents length-pos stream-pos)
            (loop (+ stream-pos (string-length "stream\n")
                     (parse-uint-at contents (+ length-pos (string-length "/Length "))))
                  (- n 1)))))))

;; Extract the two hex halves of a trailer /ID array.
(define (extract-id contents)
  (let* ((id-pos (substring-index "/ID [ <" contents 0))
         (hex1-start (+ id-pos (string-length "/ID [ <")))
         (hex1-end (substring-index ">" contents hex1-start))
         (hex1 (substring contents hex1-start hex1-end))
         (hex2-start (+ (substring-index "<" contents hex1-end) 1))
         (hex2-end (substring-index ">" contents hex2-start))
         (hex2 (substring contents hex2-start hex2-end)))
    (values hex1 hex2)))

;; ---------------------------------------------------------------------
;; pdfa: pure date/hash/XMP helpers

(test-group "pdfa: pure helpers"
  (test "D:19700101000000Z" (pdf-date-string 0))
  (test "1970-01-01T00:00:00Z" (xmp-date-string 0))
  (test "098f6bcd4621d373cade4e832627b4f6" (md5-hex "test"))
  ;; build-xmp-packet escapes & and < in field values correctly, via
  ;; SXML->HTML's own text-node handling.
  (let ((packet (build-xmp-packet title: "A & B < C")))
    (test-assert (substring-index "A &amp; B &lt; C" packet 0))
    (test-assert (not (substring-index "A & B < C" packet 0)))
    (test-assert (substring-index "<?xpacket begin=\"\" id=\"W5M0MpCehiHzreSzNTczkc9d\"?>" packet 0))
    (test-assert (substring-index "<?xpacket end=\"w\"?>" packet 0))))

;; ---------------------------------------------------------------------
;; PDF text strings for document information values

(define +srgb-icc+ "/usr/share/color/icc/sRGB.icc")

(test-group "pdf-text-string: ASCII literal strings and UTF-16BE hex strings"
  (test "(A Report)" (pdf-text-string "A Report"))
  (test "(a\\(b\\)c\\\\d)" (pdf-text-string "a(b)c\\d"))
  ;; e-acute (2 UTF-8 bytes), check mark (3) and a musical symbol above
  ;; U+FFFF (4, written as a surrogate pair)
  (test "<FEFF00430061006600E9002027130020D834DD1E>"
        (pdf-text-string "Caf\xe9; \x2713; \x1d11e;"))
  (test "()" (pdf-text-string "")))

(test-group "build-xmp-packet: PDF/A identification fields only when claimed"
  (let ((plain (build-xmp-packet title: "T"))
        (claimed (build-xmp-packet title: "T" pdfa-part: "1" pdfa-conformance: "B")))
    (test #f (substring-index "pdfaid" plain 0))
    (test-assert (substring-index "xmlns:pdfaid=\"http://www.aiim.org/pdfa/ns/id/\"" claimed 0))
    (test-assert (substring-index "<pdfaid:part>1</pdfaid:part>" claimed 0))
    (test-assert (substring-index "<pdfaid:conformance>B</pdfaid:conformance>" claimed 0))))

(test-group "set-document-info!: empty values dropped, control characters rejected"
  (let ((path (create-temporary-file "pdf")))
    (with-pdfa-document-to-file (pdfa-doc doc #f #f) path
     (set-document-info! pdfa-doc title: "" author: "Someone")
     (with-page doc (pctx) (basic-rect pctx 0 0 10 10)))
    (let ((contents (read-file path)))
      (test #f (substring-index "/Title" contents 0))
      (test #f (substring-index "dc:title" contents 0))
      (test-assert (substring-index "/Author (Someone)" contents 0)))
    (delete-file path))
  (let ((pdfa-doc (build-pdfa-document)))
    (test-error (set-document-info! pdfa-doc title: "a\x1;b"))
    (test-error (set-document-info! pdfa-doc title: 42))
    ;; tab, line feed and carriage return are allowed
    (set-document-info! pdfa-doc title: "a\tb\nc")
    (test-assert #t)))

(test-group "non-ASCII document information: /Info and XMP agree"
  (let ((path (create-temporary-file "pdf")))
    (with-pdfa-document-to-file (pdfa-doc doc #f #f) path
     (set-document-info! pdfa-doc title: "Caf\xe9; \x2713; \x1d11e;" subject: "x(y)")
     (with-page doc (pctx) (basic-rect pctx 0 0 10 10)))
    (let ((contents (read-file path)))
      (test-assert (substring-index "/Title <FEFF00430061006600E9002027130020D834DD1E>" contents 0))
      (test-assert (substring-index "/Subject (x\\(y\\))" contents 0))
      ;; the XMP copy holds the same text, UTF-8-encoded (read-file maps
      ;; each byte to one character)
      (test-assert (substring-index
                    (string #\C #\a #\f (integer->char #xC3) (integer->char #xA9) #\space
                            (integer->char #xE2) (integer->char #x9C) (integer->char #x93) #\space
                            (integer->char #xF0) (integer->char #x9D) (integer->char #x84)
                            (integer->char #x9E))
                    contents 0))
      (test-assert (substring-index "<rdf:li xml:lang=\"x-default\">x(y)</rdf:li>" contents 0)))
    (if (file-exists? "/usr/bin/pdfinfo")
        (let ((out (with-input-from-pipe (string-append "/usr/bin/pdfinfo " path)
                                         (lambda () (read-string #f)))))
          (test-assert (substring-index "Caf\xe9; \x2713; \x1d11e;" out 0)))
        (print "note: /usr/bin/pdfinfo not found; skipping pdfinfo title check"))
    (delete-file path)))

(test-group "write-pdfa-document: binary header comment and PDF/A claim"
  (let ((path (create-temporary-file "pdf")))
    (with-pdfa-document-to-file (pdfa-doc doc #f) path
     (when (file-exists? +srgb-icc+)
       (set-output-intent! pdfa-doc +srgb-icc+))
     (with-page doc (pctx) (basic-rect pctx 0 0 10 10) (stroke pctx)))
    (let ((contents (read-file path)))
      (test-assert (string-prefix? "%PDF-1.4\n" contents))
      (test '(37 226 227 207 211 10)
            (map (lambda (i) (char->integer (string-ref contents i))) '(9 10 11 12 13 14)))
      (test-assert (substring-index "<pdfaid:part>1</pdfaid:part>" contents 0))
      (test-assert (substring-index "<pdfaid:conformance>B</pdfaid:conformance>" contents 0)))
    (delete-file path))
  ;; conformance #f: same metadata, no claim
  (let ((path (create-temporary-file "pdf")))
    (with-pdfa-document-to-file (pdfa-doc doc #f #f) path
     (with-page doc (pctx) (basic-rect pctx 0 0 10 10) (stroke pctx)))
    (let ((contents (read-file path)))
      (test #f (substring-index "pdfaid" contents 0))
      (test-assert (substring-index "<?xpacket begin=" contents 0)))
    (delete-file path))
  (test-error (build-pdfa-document #t 'pdfa-2b)))

;; ---------------------------------------------------------------------
;; with-pdfa-document-to-file: /Info, /Metadata, /ID presence and absence

(test-group "with-pdfa-document-to-file: /Info, /Metadata, /ID presence and absence"
  ;; no set-document-info! call: no /Info in the trailer, but /Metadata
  ;; is still present in the catalog (always emitted), and a well-formed
  ;; /ID with two equal 32-hex-char values
  (let ((path (create-temporary-file "pdf")))
    (with-pdfa-document-to-file (pdfa-doc doc #t #f) path
     (let ((helvetica (build-font doc "Helvetica")))
       (with-page doc (pctx)
        (in-text-mode pctx (set-font pctx (font-name helvetica) 24)
                           (move-text pctx 72 700) (draw-text pctx "no info")))))
    (let ((contents (read-file path)))
      (test 0 (count-occurrences "/Info " contents))
      ;; "/Metadata " also matches the stream object's own
      ;; "/Type /Metadata /Subtype /XML" entry, not just the catalog's
      ;; reference to it -- a presence check is what's meaningful here.
      (test-assert (< 0 (count-occurrences "/Metadata " contents)))
      (test-assert (substring-index "/ID [ <" contents 0))
      (let-values (((hex1 hex2) (extract-id contents)))
        (test 32 (string-length hex1))
        (test hex1 hex2)))
    (delete-file path))
  ;; set-document-info! called: /Info N G R in the trailer, and the
  ;; referenced object's dictionary contains the expected values
  (let ((path (create-temporary-file "pdf")))
    (with-pdfa-document-to-file (pdfa-doc doc #t #f) path
     (set-document-info! pdfa-doc title: "A Report" author: "I. Raikov")
     (let ((helvetica (build-font doc "Helvetica")))
       (with-page doc (pctx)
        (in-text-mode pctx (set-font pctx (font-name helvetica) 24)
                           (move-text pctx 72 700) (draw-text pctx "with info")))))
    (let ((contents (read-file path)))
      (test 1 (count-occurrences "/Info " contents))
      (test-assert (substring-index "/Title (A Report)" contents 0))
      (test-assert (substring-index "/Author (I. Raikov)" contents 0)))
    (delete-file path)))

;; ---------------------------------------------------------------------
;; two documents built back-to-back in the same process must not share
;; any state: distinct trailer /ID values, confirming the /ID hash no
;; longer depends on any shared counter or global.

(test-group "with-pdfa-document-to-file: sequential documents get distinct /ID values"
  (define (build-and-get-id)
    (let ((path (create-temporary-file "pdf")))
      (with-pdfa-document-to-file (pdfa-doc doc #t #f) path
       (let ((helvetica (build-font doc "Helvetica")))
         (with-page doc (pctx)
          (in-text-mode pctx (set-font pctx (font-name helvetica) 24)
                             (move-text pctx 72 700) (draw-text pctx "id check")))))
      (let-values (((hex1 hex2) (extract-id (read-file path))))
        (delete-file path)
        hex1)))
  (test-assert (not (equal? (build-and-get-id) (build-and-get-id)))))

;; ---------------------------------------------------------------------
;; with-pdfa-document-to-file: /Metadata stream is never filtered, even
;; when compression is on (its default) -- PDF/A requires the XMP stream
;; to always be stored uncompressed.

(test-group "with-pdfa-document-to-file: /Metadata stream is never filtered"
  (let ((path (create-temporary-file "pdf")))
    (with-pdfa-document-to-file (pdfa-doc doc #t #f) path
     (set-document-info! pdfa-doc title: "Filter check")
     (let ((helvetica (build-font doc "Helvetica")))
       (with-page doc (pctx)
        (in-text-mode pctx (set-font pctx (font-name helvetica) 24)
                           (move-text pctx 72 700) (draw-text pctx "x")))))
    (let* ((contents (read-file path))
           ;; write order: content stream (occurrence 0), /Metadata (1)
           (dict-text (stream-dict-text contents 1)))
      (test-assert (not (substring-index "/Filter" dict-text 0)))
      ;; the XMP text is directly readable without flate-decompress
      (let ((xmp-bytes (extract-stream-bytes contents 1)))
        (test-assert (substring-index "<?xpacket begin=" xmp-bytes 0))
        (test-assert (substring-index "Filter check" xmp-bytes 0))))
    (delete-file path)))

;; ---------------------------------------------------------------------
;; with-pdfa-document-to-file: /OutputIntents presence and absence

(test-group "with-pdfa-document-to-file: /OutputIntents presence and absence"
  ;; no set-output-intent! call: no /OutputIntents key at all
  (let ((path (create-temporary-file "pdf")))
    (with-pdfa-document-to-file (pdfa-doc doc #t #f) path
     (let ((helvetica (build-font doc "Helvetica")))
       (with-page doc (pctx)
        (in-text-mode pctx (set-font pctx (font-name helvetica) 24)
                           (move-text pctx 72 700) (draw-text pctx "no intent")))))
    (let ((contents (read-file path)))
      (test 0 (count-occurrences "/OutputIntents" contents)))
    (delete-file path))
  ;; set-output-intent! called (best-effort: only if the real sRGB
  ;; fixture on this machine is present)
  (if (file-exists? "/usr/share/color/icc/sRGB.icc")
      (let ((path (create-temporary-file "pdf")))
        (with-pdfa-document-to-file (pdfa-doc doc #t #f) path
         (set-output-intent! pdfa-doc "/usr/share/color/icc/sRGB.icc")
         (let ((helvetica (build-font doc "Helvetica")))
           (with-page doc (pctx)
            (in-text-mode pctx (set-font pctx (font-name helvetica) 24)
                               (move-text pctx 72 700) (draw-text pctx "with intent")))))
        (let ((contents (read-file path)))
          (test 1 (count-occurrences "/OutputIntents" contents))
          (test-assert (substring-index "/S /GTS_PDFA1" contents 0))
          (test-assert (substring-index "/N 3" contents 0)))
        (delete-file path))
      (print "note: /usr/share/color/icc/sRGB.icc not found; skipping /OutputIntents presence check")))

;; ---------------------------------------------------------------------
;; output intent: ICC profile round-trip, using the real sRGB fixture on
;; this machine, best-effort/skip if absent

(test-group "output intent: ICC profile round-trip"
  (if (file-exists? "/usr/share/color/icc/sRGB.icc")
      (let ((path (create-temporary-file "pdf")))
        (with-pdfa-document-to-file (pdfa-doc doc #t #f) path
         (set-output-intent! pdfa-doc "/usr/share/color/icc/sRGB.icc")
         (let ((helvetica (build-font doc "Helvetica")))
           (with-page doc (pctx)
            (in-text-mode pctx (set-font pctx (font-name helvetica) 24)
                               (move-text pctx 72 700) (draw-text pctx "output intent test")))))
        (let* ((contents (read-file path))
               (original (read-file "/usr/share/color/icc/sRGB.icc"))
               ;; stream write order: content stream (0), /Metadata (1),
               ;; ICC profile (2) -- confirmed against the actual
               ;; generated file, not assumed.
               (icc-bytes-in-pdf (extract-stream-bytes contents 2))
               (decompressed (if (substring-index "/Filter /FlateDecode" contents 0)
                                  (flate-decompress icc-bytes-in-pdf)
                                  icc-bytes-in-pdf)))
          (test original decompressed))
        (delete-file path))
      (print "note: /usr/share/color/icc/sRGB.icc not found; skipping ICC round-trip test")))

;; ---------------------------------------------------------------------
;; end-to-end well-formedness with document info, output intent,
;; compression, a page and a font all enabled together

(test-group "with-pdfa-document-to-file: end-to-end well-formedness with everything enabled"
  (let ((path (create-temporary-file "pdf")))
    (with-pdfa-document-to-file (pdfa-doc doc #t #f) path
     (set-document-info! pdfa-doc title: "Full Test" author: "Test Suite")
     (when (file-exists? "/usr/share/color/icc/sRGB.icc")
       (set-output-intent! pdfa-doc "/usr/share/color/icc/sRGB.icc"))
     (let ((helvetica (build-font doc "Helvetica")))
       (with-page doc (pctx)
        (in-text-mode pctx (set-font pctx (font-name helvetica) 24)
                           (move-text pctx 72 700) (draw-text pctx "everything enabled")))))
    (let ((contents (read-file path)))
      (test-assert (string-prefix? "%PDF-1.4\n" contents))
      (test-assert (string-suffix? "%%EOF\n" contents)))
    (if (file-exists? "/usr/bin/pdftotext")
        (let ((out (with-input-from-pipe
                    (string-append "/usr/bin/pdftotext " path " -")
                    (lambda () (read-string #f)))))
          (test-assert (substring-index "everything enabled" out 0)))
        (print "note: /usr/bin/pdftotext not found; skipping text-extraction check"))
    (delete-file path)))

;; ---------------------------------------------------------------------
;; plain pdf usage (no pdfa macro involved) is unaffected by
;; importing pdfa alongside pdf in the same file

(test-group "plain with-document-to-file is unaffected by importing pdfa"
  (let ((path (create-temporary-file "pdf")))
    (with-document-to-file (doc) path
     (let ((helvetica (build-font doc "Helvetica")))
       (with-page doc (pctx)
        (in-text-mode pctx (set-font pctx (font-name helvetica) 24)
                           (move-text pctx 72 700) (draw-text pctx "plain")))))
    (let ((contents (read-file path)))
      (test-assert (string-prefix? "%PDF-1.3\n" contents))
      (test 0 (count-occurrences "/Info " contents))
      (test 0 (count-occurrences "/Metadata" contents))
      (test 0 (count-occurrences "/ID [" contents)))
    (delete-file path)))


;; ---------------------------------------------------------------------
;; Conformance mode: check-pdfa-conformance and write-time errors

(define +liberation-ttf+ "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf")

;; Builds a PDF/A document (conformance mode on), runs body on it, and
;; returns the kinds of the violations check-pdfa-conformance finds.
(define-syntax violation-kinds
  (syntax-rules ()
    ((_ (pdfa-doc doc) body ...)
     (let* ((pdfa-doc (build-pdfa-document #f))
            (doc (pdfa-document-doc pdfa-doc)))
       body ...
       (map pdfa-violation-kind (check-pdfa-conformance pdfa-doc))))))

(test-group "check-pdfa-conformance: a conforming document has no violations"
  (if (and (file-exists? +srgb-icc+) (file-exists? +liberation-ttf+))
      (begin
        (test '()
              (violation-kinds (pdfa-doc doc)
                (set-output-intent! pdfa-doc +srgb-icc+)
                (let ((font (build-embedded-font doc +liberation-ttf+))
                      (opaque (build-ext-gstate doc 1.0 1.0 "/Normal")))
                  (with-page doc (pctx)
                   (set-ext-gstate pctx (ext-gstate-name opaque))
                   (set-rgb-fill pctx 0.2 0.4 0.6)
                   (set-gray-stroke pctx 0.5)
                   (basic-rect pctx 10 10 100 100)
                   (fill-and-stroke pctx)
                   (in-text-mode pctx (set-font pctx (embedded-font-name font) 12)
                                      (move-text pctx 72 700)
                                      ;; operator names inside strings are not operators
                                      (draw-encoded-text pctx font "k K (k) \\ rg"))))))
        ;; ... and writes a file that claims PDF/A-1b
        (let ((path (create-temporary-file "pdf")))
          (with-pdfa-document-to-file (pdfa-doc doc) path
           (set-output-intent! pdfa-doc +srgb-icc+)
           (let ((font (build-embedded-font doc +liberation-ttf+)))
             (with-page doc (pctx)
              (in-text-mode pctx (set-font pctx (embedded-font-name font) 12)
                                 (move-text pctx 72 700)
                                 (draw-encoded-text pctx font "conforming")))))
          (test-assert (file-contains? path "<pdfaid:conformance>B</pdfaid:conformance>"))
          (delete-file path)))
      (print "note: sRGB profile or Liberation Sans not found; skipping")))

(test-group "check-pdfa-conformance: missing output intent"
  (test '(missing-output-intent)
        (violation-kinds (pdfa-doc doc)
          (with-page doc (pctx) (basic-rect pctx 0 0 10 10) (stroke pctx))))
  ;; without an output intent, every device color operator is reported
  (test '(missing-output-intent color-space-mismatch color-space-mismatch color-space-mismatch)
        (violation-kinds (pdfa-doc doc)
          (with-page doc (pctx)
           (set-gray-fill pctx 0.5) (set-rgb-fill pctx 1 0 0) (set-cmyk-fill pctx 0 0 0 1)
           (basic-rect pctx 0 0 10 10) (fill-path pctx)))))

(test-group "check-pdfa-conformance: unembedded base-14 font"
  (if (file-exists? +srgb-icc+)
      (let ((pdfa-doc (build-pdfa-document #f)))
        (set-output-intent! pdfa-doc +srgb-icc+)
        (let ((doc (pdfa-document-doc pdfa-doc)))
          (build-font doc "Helvetica")
          (with-page doc (pctx) (basic-rect pctx 0 0 10 10) (stroke pctx)))
        (let ((vs (check-pdfa-conformance pdfa-doc)))
          (test '(unembedded-font) (map pdfa-violation-kind vs))
          (test-assert (substring-index "/Helvetica" (pdfa-violation->string (car vs)) 0))))
      (print "note: sRGB profile not found; skipping")))

(test-group "check-pdfa-conformance: device color must match the output intent"
  (if (file-exists? +srgb-icc+)
      (begin
        (test '(color-space-mismatch color-space-mismatch)
              (violation-kinds (pdfa-doc doc)
                (set-output-intent! pdfa-doc +srgb-icc+)
                (with-page doc (pctx)
                 (set-cmyk-fill pctx 0 0 0 1)
                 (set-cmyk-stroke pctx 0 0 0 1)
                 ;; repeated uses of the same operator are reported once
                 (set-cmyk-fill pctx 0 1 0 0)
                 (basic-rect pctx 0 0 10 10) (fill-path pctx))))
        (test '()
              (violation-kinds (pdfa-doc doc)
                (set-output-intent! pdfa-doc +srgb-icc+)
                (with-page doc (pctx)
                 (set-rgb-fill pctx 1 0 0) (set-gray-fill pctx 0.5)
                 (basic-rect pctx 0 0 10 10) (fill-path pctx)))))
      (print "note: sRGB profile not found; skipping")))

(test-group "check-pdfa-conformance: transparency in graphics states"
  (if (file-exists? +srgb-icc+)
      (test '(transparency transparency transparency)
            (violation-kinds (pdfa-doc doc)
              (set-output-intent! pdfa-doc +srgb-icc+)
              (build-ext-gstate doc 0.5 0.75 "/Multiply")
              (with-page doc (pctx) (basic-rect pctx 0 0 10 10) (stroke pctx))))
      (print "note: sRGB profile not found; skipping")))

(test-group "check-pdfa-conformance: implementation limits"
  (if (file-exists? +srgb-icc+)
      (begin
        (test '(limit-exceeded)
              (violation-kinds (pdfa-doc doc)
                (set-output-intent! pdfa-doc +srgb-icc+)
                (with-page doc (pctx)
                 (let loop ((i 0)) (when (< i 30) (save-graphics-state pctx) (loop (+ i 1))))
                 (let loop ((i 0)) (when (< i 30) (restore-graphics-state pctx) (loop (+ i 1)))))))
        (test '(limit-exceeded)
              (violation-kinds (pdfa-doc doc)
                (set-output-intent! pdfa-doc +srgb-icc+)
                (with-page doc (pctx) (move-to pctx 40000 10) (line-to pctx 0 0) (stroke pctx))))
        (test '(limit-exceeded)
              (violation-kinds (pdfa-doc doc)
                (set-output-intent! pdfa-doc +srgb-icc+)
                (with-page doc (pctx)
                 (in-text-mode pctx (draw-text pctx (make-string 70000 #\a))))))
        ;; 28 levels of nesting are allowed
        (test '()
              (violation-kinds (pdfa-doc doc)
                (set-output-intent! pdfa-doc +srgb-icc+)
                (with-page doc (pctx)
                 (let loop ((i 0)) (when (< i 28) (save-graphics-state pctx) (loop (+ i 1))))
                 (let loop ((i 0)) (when (< i 28) (restore-graphics-state pctx) (loop (+ i 1))))))))
      (print "note: sRGB profile not found; skipping")))

(test-group "check-pdfa-conformance: several violations are all reported, in order"
  (test '(missing-output-intent unembedded-font unembedded-font)
        (violation-kinds (pdfa-doc doc)
          (build-font doc "Helvetica")
          (build-font doc "Times-Roman")
          (with-page doc (pctx) (basic-rect pctx 0 0 10 10) (stroke pctx)))))

(test-group "write-pdfa-document: a non-conforming document raises and writes nothing"
  (let ((path (string-append (create-temporary-file "dir") ".never.pdf")))
    (test-error (with-pdfa-document-to-file (pdfa-doc doc) path
                 (let ((helvetica (build-font doc "Helvetica")))
                   (with-page doc (pctx)
                    (in-text-mode pctx (set-font pctx (font-name helvetica) 12)
                                       (draw-text pctx "no"))))))
    (test #f (file-exists? path))
    ;; the error message lists every violation
    (test-assert
     (handle-exceptions exn
       (let ((msg ((condition-property-accessor 'exn 'message) exn)))
         (and (substring-index "no output intent" msg 0)
              (substring-index "is not embedded" msg 0)))
       (with-pdfa-document-to-file (pdfa-doc doc) path
        (build-font doc "Helvetica")
        (with-page doc (pctx) (basic-rect pctx 0 0 1 1)))
       #f))))

(test-group "set-output-intent!: ICC profile header checks"
  (let ((bad (create-temporary-file "icc")))
    (call-with-output-file bad (lambda (p) (write-string "not an ICC profile" p)))
    (test-error (set-output-intent! (build-pdfa-document) bad))
    ;; without conformance mode the profile is accepted as given
    (set-output-intent! (build-pdfa-document #t #f) bad)
    (delete-file bad))
  (if (file-exists? +srgb-icc+)
      (let ((bv (call-with-input-file +srgb-icc+
                  (lambda (p) (read-bytevector (file-size +srgb-icc+) p)))))
        (test '() (icc-profile-problems bv 3))
        (test 1 (length (icc-profile-problems bv 4)))
        (test-error (set-output-intent! (build-pdfa-document) +srgb-icc+ n: 4)))
      (print "note: sRGB profile not found; skipping")))

;; Writes a copy of Liberation Sans whose OS/2 fsType says
;; "restricted licence", to path.
(define (write-restricted-font-copy path)
  (let* ((bv (call-with-input-file +liberation-ttf+
               (lambda (p) (read-bytevector (file-size +liberation-ttf+) p))))
         (u16 (lambda (i) (+ (* 256 (bytevector-u8-ref bv i)) (bytevector-u8-ref bv (+ i 1)))))
         (u32 (lambda (i) (+ (* 65536 (u16 i)) (u16 (+ i 2)))))
         (os2 (let loop ((k 0))
                (let ((rec (+ 12 (* 16 k))))
                  (if (equal? (map (lambda (j) (bytevector-u8-ref bv (+ rec j))) '(0 1 2 3))
                              (map char->integer (string->list "OS/2")))
                      (u32 (+ rec 8))
                      (loop (+ k 1)))))))
    (bytevector-u8-set! bv (+ os2 8) 0)
    (bytevector-u8-set! bv (+ os2 9) 2)
    (call-with-output-file path (lambda (p) (write-bytevector bv p)))))

(test-group "check-pdfa-conformance: font licence forbids embedding"
  (if (and (file-exists? +srgb-icc+) (file-exists? +liberation-ttf+))
      (let ((ttf (create-temporary-file "ttf")))
        (write-restricted-font-copy ttf)
        (test '(restricted-font-license)
              (violation-kinds (pdfa-doc doc)
                (set-output-intent! pdfa-doc +srgb-icc+)
                (let ((font (build-embedded-font doc ttf)))
                  (test-assert (embedded-font-restricted? font))
                  (with-page doc (pctx) (basic-rect pctx 0 0 10 10) (stroke pctx)))))
        (delete-file ttf))
      (print "note: sRGB profile or Liberation Sans not found; skipping")))

(test-exit)
