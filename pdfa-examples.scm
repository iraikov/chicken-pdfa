;;;
;;; pdfa-examples.scm: how to produce a PDF/A-1b file with the pdfa egg.
;;;
;;; A PDF/A-1b file must look the same on every viewer, now and decades
;;; from now. That means the file carries everything needed to draw it:
;;; its fonts, a color profile saying what its colors mean, and
;;; metadata describing it. This example builds a one-page report that
;;; meets those rules, and shows how to find out why a document does not.
;;;
;;; Usage:
;;;
;;;   csi -s pdfa-examples.scm [FONT.ttf [PROFILE.icc]]
;;;
;;; FONT.ttf is any TrueType font whose licence allows embedding, and
;;; PROFILE.icc an RGB ICC profile (version 2). They default to Liberation
;;; Sans and the sRGB profile found on most Linux systems. The example
;;; writes pdfa-example.pdf in the current directory.
;;;
;;; To validate the result, run veraPDF (https://verapdf.org):
;;;
;;;   verapdf --flavour 1b pdfa-example.pdf
;;;

(import scheme (chicken base) (chicken format) (chicken string)
        (chicken process-context)
        pdf pdfa pdf-truetype)

(define default-font "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf")
(define default-profile "/usr/share/color/icc/sRGB.icc")

(define font-path
  (let ((args (command-line-arguments)))
    (if (pair? args) (car args) default-font)))

(define profile-path
  (let ((args (command-line-arguments)))
    (if (and (pair? args) (pair? (cdr args))) (cadr args) default-profile)))

;; ---------------------------------------------------------------------
;; Text layout helpers. Widths come from the embedded font itself, so
;; centring and line breaking match exactly what a viewer will draw.

;; Draws text centered on x at baseline y.
(define (draw-centered pctx font size x y text)
  (in-text-mode pctx
   (set-font pctx (embedded-font-name font) size)
   (move-text pctx (- x (/ (embedded-font-string-width font text size) 2)) y)
   (draw-encoded-text pctx font text)))

;; Breaks text into lines no wider than width at the given size,
;; breaking only at spaces. Returns the list of lines.
(define (wrap-text font size width text)
  (let loop ((words (string-split text " ")) (line "") (lines '()))
    (cond ((null? words)
           (reverse (if (string=? line "") lines (cons line lines))))
          ((string=? line "")
           (loop (cdr words) (car words) lines))
          (else
           (let ((candidate (string-append line " " (car words))))
             (if (<= (embedded-font-string-width font candidate size) width)
                 (loop (cdr words) candidate lines)
                 (loop words "" (cons line lines))))))))

;; Draws text as a left-aligned paragraph starting at (x, y), wrapped to
;; width, with the given line spacing. Returns the baseline below the
;; paragraph's last line.
(define (draw-paragraph pctx font size x y width leading text)
  (let ((lines (wrap-text font size width text)))
    (in-text-mode pctx
     (set-font pctx (embedded-font-name font) size)
     (set-text-leading pctx leading)
     (move-text pctx x y)
     (draw-encoded-text pctx font (car lines))
     (for-each (lambda (line) (draw-encoded-text-on-next-line pctx font line))
               (cdr lines)))
    (- y (* leading (length lines)))))

;; ---------------------------------------------------------------------
;; The report.

(define quarterly-sales
  '(("Q1" . 0.42) ("Q2" . 0.65) ("Q3" . 0.58) ("Q4" . 0.91)))

;; Draws a bar chart of quarterly-sales with its lower-left corner at
;; (x, y). Colors are DeviceRGB, which PDF/A-1 allows because the
;; document's output intent is an RGB profile.
(define (draw-bar-chart pctx font x y width height)
  (let* ((n (length quarterly-sales))
         (slot (/ width n))
         (bar (* slot 0.6)))
    ;; axes, in gray
    (set-gray-stroke pctx 0.3)
    (set-line-width pctx 1)
    (move-to pctx x (+ y height))
    (line-to pctx x y)
    (line-to pctx (+ x width) y)
    (stroke pctx)
    ;; one bar and label per quarter
    (let loop ((items quarterly-sales) (i 0))
      (unless (null? items)
        (let ((left (+ x (* i slot) (/ (- slot bar) 2)))
              (h (* height (cdar items))))
          (set-rgb-fill pctx 0.16 0.38 0.62)
          (basic-rect pctx left y bar h)
          (fill-path pctx)
          (set-gray-fill pctx 0)
          (draw-centered pctx font 10 (+ left (/ bar 2)) (- y 14) (caar items))
          (draw-centered pctx font 9 (+ left (/ bar 2)) (+ y h 4)
                        (sprintf "~a%" (inexact->exact (round (* 100 (cdar items))))))
          (loop (cdr items) (+ i 1)))))))

;; Writes pdfa-example.pdf.
(define (pdfa-example)
  (with-pdfa-document-to-file (pdfa-doc doc) "pdfa-example.pdf"

    ;; Metadata. Written twice, as the /Info dictionary and as an XMP
    ;; packet, with the same values; non-ASCII text is accepted in both.
    (set-document-info! pdfa-doc
                        title: "Quarterly report \x2014; caf\xe9; division"
                        author: "Example Author"
                        subject: "A PDF/A-1b example"
                        keywords: "PDF/A, archiving, example"
                        creator: "pdfa-examples.scm")

    ;; Output intent. Every color in the file is interpreted through
    ;; this ICC profile, so its meaning does not depend on the device.
    (set-output-intent! pdfa-doc profile-path)

    ;; Fonts. Every font must be embedded; build-embedded-font embeds
    ;; the TrueType file and takes all metrics from it. The base-14 fonts
    ;; from build-font are not embedded and are rejected.
    (let ((font (build-embedded-font doc font-path)))

      (with-page doc (pctx)
        (let ((width (page-width pctx))
              (height (page-height pctx)))

          ;; Text. draw-encoded-text maps each character to its
          ;; WinAnsi code and escapes it, and raises an error for any
          ;; character the font cannot draw, instead of printing a blank.
          (set-gray-fill pctx 0)
          (draw-centered pctx font 22 (/ width 2) (- height 90) "Quarterly report")
          (draw-centered pctx font 12 (/ width 2) (- height 112)
                         "Caf\xe9; division \x2014; \x201c;archival edition\x201d;")

          (let ((below
                 (draw-paragraph
                  pctx font 11 72 (- height 160) (- width 144) 15
                  (string-append
                   "This page is a PDF/A-1b document. It embeds its font, "
                   "declares the color profile its colors refer to, and "
                   "describes itself in XMP metadata, so that it can be "
                   "reproduced exactly in the future without relying on "
                   "fonts or settings of the system that opens it. Sales "
                   "rose to \x20ac;1.2 million in the fourth quarter."))))

            ;; Graphics. Vector drawing in DeviceRGB and DeviceGray.
            ;; Transparency (alpha or blend modes) is not allowed.
            (draw-bar-chart pctx font 120 (- below 260) (- width 240) 200))

          (draw-centered pctx font 8 (/ width 2) 40
                         "Produced with CHICKEN Scheme and libraries pdf, pdf-truetype and pdfa"))))))

;; ---------------------------------------------------------------------
;; Finding out why a document does not conform.
;;
;; with-pdfa-document-to-file checks the document before writing it,
;; raises an error listing every problem, and writes no file. To inspect
;; the problems yourself, build the document with build-pdfa-document
;; and call check-pdfa-conformance, which returns them as a list.

(define (conformance-report-example)
  (let* ((pdfa-doc (build-pdfa-document))
         (doc (pdfa-document-doc pdfa-doc))
         (helvetica (build-font doc "Helvetica")))   ; not embedded
    ;; no set-output-intent! call
    (with-page doc (pctx)
      (set-cmyk-fill pctx 0 0.8 0.8 0)                 ; CMYK, with no CMYK intent
      (in-text-mode pctx
       (set-font pctx (font-name helvetica) 12)
       (move-text pctx 72 700)
       (draw-text pctx "This document breaks three PDF/A-1b rules.")))
    (print "Problems found by check-pdfa-conformance:")
    (for-each (lambda (v) (print "  " (pdfa-violation->string v)))
              (check-pdfa-conformance pdfa-doc))))

(pdfa-example)
(print "wrote pdfa-example.pdf")
(newline)
(conformance-report-example)
