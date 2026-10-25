;;;
;;; verapdf-fixtures.scm -- writes the documents that verapdf-check.sh
;;; runs through the veraPDF validator.
;;;
;;; Usage: csi -s verapdf-fixtures.scm OUTPUT-DIRECTORY
;;;
;;; Each fixture is written as OUTPUT-DIRECTORY/<name>.pdf. A name
;;; starting with "pass-" must be accepted as PDF/A-1b; a name starting
;;; with "fail-" must be rejected, which shows that the harness can see
;;; a failure at all.
;;;
;;; Two environment variables name the input files some fixtures need:
;;;
;;;   PDFA_ICC_PROFILE   an RGB ICC profile (version 2), such as sRGB
;;;   PDFA_TTF_FONT      a TrueType font whose licence allows embedding
;;;
;;; A fixture whose input file is not set, or does not exist, is skipped
;;; with a note.
;;;

(import scheme (chicken base) (chicken file) (chicken process-context) (chicken string) srfi-1
        pdf pdfa pdf-truetype)

;; Returns the value of environment variable var, or #f when it is unset
;; or empty.
(define (input-file var)
  (let ((v (get-environment-variable var)))
    (and v (not (string=? v "")) v)))

(define +icc-profile+ (input-file "PDFA_ICC_PROFILE"))
(define +ttf-font+ (input-file "PDFA_TTF_FONT"))

(define out-dir
  (let ((args (command-line-arguments)))
    (if (pair? args)
        (car args)
        (error 'verapdf-fixtures "usage: csi -s verapdf-fixtures.scm OUTPUT-DIRECTORY"))))

(define (fixture-path name)
  (string-append out-dir "/" name ".pdf"))

;; Runs thunk to write fixture `name` when every environment variable in
;; `needs` names an existing file; otherwise prints a note naming the
;; variables that do not.
(define (fixture name needs thunk)
  (let ((missing (filter (lambda (var)
                           (let ((f (input-file var)))
                             (not (and f (file-exists? f)))))
                         needs)))
    (if (null? missing)
        (begin (thunk (fixture-path name))
               (print "wrote " name))
        (print "note: skipping " name "; set " (string-intersperse missing " and ")
               (if (null? (cdr missing)) " to an existing file" " to existing files")))))

;; 1. Metadata and output intent, one empty page.
(fixture "pass-1-metadata" (list "PDFA_ICC_PROFILE")
  (lambda (path)
    (with-pdfa-document-to-file (pdfa-doc doc) path
     (set-document-info! pdfa-doc title: "Metadata only" author: "pdfa test suite"
                         subject: "veraPDF fixture 1" keywords: "pdfa, test"
                         creator: "verapdf-fixtures.scm")
     (set-output-intent! pdfa-doc +icc-profile+)
     (with-page doc (pctx) (end-path-no-op pctx)))))

;; 2. One page of ASCII text in an embedded TrueType font.
(fixture "pass-2-embedded-font" (list "PDFA_ICC_PROFILE" "PDFA_TTF_FONT")
  (lambda (path)
    (with-pdfa-document-to-file (pdfa-doc doc) path
     (set-document-info! pdfa-doc title: "Embedded font")
     (set-output-intent! pdfa-doc +icc-profile+)
     (let ((font (build-embedded-font doc +ttf-font+)))
       (with-page doc (pctx)
        (in-text-mode pctx
         (set-font pctx (embedded-font-name font) 24)
         (move-text pctx 72 700)
         (draw-encoded-text pctx font "Hello, PDF/A-1b (with parentheses).")))))))

;; 3. Non-ASCII document information and WinAnsi text: e-acute, the euro
;; sign and an em dash in the page text; e-acute, a check mark and a
;; character above U+FFFF in the title.
(fixture "pass-3-winansi-text" (list "PDFA_ICC_PROFILE" "PDFA_TTF_FONT")
  (lambda (path)
    (with-pdfa-document-to-file (pdfa-doc doc) path
     (set-document-info! pdfa-doc title: "Caf\xe9; \x2713; \x1d11e;" author: "Ren\xe9;e")
     (set-output-intent! pdfa-doc +icc-profile+)
     (let ((font (build-embedded-font doc +ttf-font+)))
       (with-page doc (pctx)
        (in-text-mode pctx
         (set-font pctx (embedded-font-name font) 18)
         (move-text pctx 72 700)
         (draw-encoded-text pctx font "Caf\xe9; \x2014; 5 \x20ac; \x2018;quoted\x2019;")))))))

;; 4. RGB and gray vector graphics under an RGB output intent.
(fixture "pass-4-vector-graphics" (list "PDFA_ICC_PROFILE")
  (lambda (path)
    (with-pdfa-document-to-file (pdfa-doc doc) path
     (set-document-info! pdfa-doc title: "Vector graphics")
     (set-output-intent! pdfa-doc +icc-profile+)
     (with-page doc (pctx)
      (set-rgb-fill pctx 0.8 0.1 0.1)
      (basic-rect pctx 72 500 200 100)
      (fill-path pctx)
      (set-gray-stroke pctx 0.4)
      (set-line-width pctx 2)
      (circle pctx 300 300 80)
      (stroke pctx)))))

;; 5. Negative case: a non-embedded base-14 font with no output intent,
;; written with the checks turned off. veraPDF must reject it.
(fixture "fail-5-unembedded-font" '()
  (lambda (path)
    (with-pdfa-document-to-file (pdfa-doc doc #t #f) path
     (let ((helvetica (build-font doc "Helvetica")))
       (with-page doc (pctx)
        (in-text-mode pctx
         (set-font pctx (font-name helvetica) 24)
         (move-text pctx 72 700)
         (draw-text pctx "not PDF/A")))))))
