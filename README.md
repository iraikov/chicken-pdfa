# chicken-pdfa

PDF/A-1b documents for [CHICKEN Scheme](https://call-cc.org).

`pdfa` is a CHICKEN egg that builds archival PDF files. It sits on top
of the `pdf` egg, which draws pages, and the `pdf-truetype` egg, which
embeds TrueType fonts. `pdfa` adds what PDF/A-1b (ISO 19005-1:2005,
level B) asks for on top of an ordinary PDF:

- a document information dictionary (`/Info`) and a matching XMP
  metadata packet, with full Unicode support in both;
- an ICC output intent that says what the document's colors mean;
- the binary header comment and the trailer `/ID`;
- a conformance check that runs before the file is written. It reports
  every rule the document breaks and writes no file if any rule fails.

The XMP packet claims PDF/A-1b conformance only for a document that
passed the check.

## Requirements

- CHICKEN 6 eggs: `pdf`, `pdf-truetype`, `srfi-1`, `srfi-13`, `message-digest`,
  `md5`, `sxml-transforms`, `datatype`
- For the tests: the `test` egg
- Optional: [veraPDF](https://verapdf.org), to validate the output
  with an independent checker

## Quick start

```scheme
(import pdf pdfa pdf-truetype)

(with-pdfa-document-to-file (pdfa-doc doc) "hello.pdf"
  (set-document-info! pdfa-doc
                      title: "Hello, archive"
                      author: "A. Author"
                      creator: "hello.scm")
  (set-output-intent! pdfa-doc "/usr/share/color/icc/sRGB.icc")
  (let ((font (build-embedded-font
               doc "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf")))
    (with-page doc (pctx)
      (in-text-mode pctx
        (set-font pctx (embedded-font-name font) 24)
        (move-text pctx 72 720)
        (draw-encoded-text pctx font "Hello, archive")))))
```

The macro binds two names:

- `pdfa-doc` is the PDF/A document. Pass it to `set-document-info!` and
  `set-output-intent!`.
- `doc` is the plain `pdf` document inside it. Pass it to ordinary
  `pdf` and `pdf-truetype` calls such as `with-page` and
  `build-embedded-font`.

A fuller example, [pdfa-examples.scm](pdfa-examples.scm), lays out a
one-page report with wrapped text and a bar chart. It also shows how to
list the problems in a document that does not conform:

```bash
csi -s pdfa-examples.scm [FONT.ttf [PROFILE.icc]]
```

## What PDF/A-1b requires of your document

`pdfa` writes the metadata for you, but some rules depend on what you
draw. The conformance check enforces these:

| Rule | What to do |
| --- | --- |
| Output intent (6.2.2) | Call `set-output-intent!` with a version 2 ICC profile (RGB, CMYK or gray). |
| Device colors (6.2.3.3) | Use only the color space of the output intent. With an RGB profile, use RGB and gray colors, not CMYK. |
| Fonts (6.3.4) | Embed every font with `build-embedded-font`. The base-14 fonts from `build-font` are not embedded and fail the check. The font's licence must allow embedding. |
| Transparency (6.4) | Do not use alpha values other than 1, blend modes other than Normal, or soft masks. |
| Limits (6.1.12) | Keep numbers, strings, names and `q` nesting inside the PDF/A-1 limits. |

Numbers in parentheses are clause numbers in ISO 19005-1:2005. The
error messages use the same numbers.

## API

### Building and writing documents

| Procedure | Purpose |
| --- | --- |
| `(with-pdfa-document-to-file (pdfa-doc doc [compress? [conformance]]) filename body ...)` | Builds a document, runs `body`, checks it and writes it. |
| `(build-pdfa-document [compress? [conformance]])` | Returns a new PDF/A document. `conformance` is `'pdfa-1b` (the default) or `#f`. |
| `(pdfa-document-doc pdfa-doc)` | Returns the plain `pdf` document inside `pdfa-doc`. |
| `(set-document-info! pdfa-doc #!key title author subject keywords creator)` | Records the document's descriptive fields. Empty fields are left out. |
| `(set-output-intent! pdfa-doc icc-path #!key n condition-identifier info)` | Embeds an ICC profile as the output intent. Defaults describe sRGB. |
| `(write-pdfa-document pdfa-doc filename)` | Checks the document, adds its metadata and writes it. |

With `conformance` set to `#f`, the library writes the same metadata
but runs no checks and makes no PDF/A claim.

### Checking conformance

| Procedure | Purpose |
| --- | --- |
| `(check-pdfa-conformance pdfa-doc)` | Returns a list of violations. The list is empty when the document conforms. |
| `(pdfa-violation-kind v)` | Returns the kind of violation as a symbol, for example `'unembedded-font`. |
| `(pdfa-violation->string v)` | Describes the violation in one line. |
| `(icc-profile-problems bytevector n)` | Returns the reasons an ICC profile cannot be used as a PDF/A-1 output intent. |

The violation kinds are `missing-output-intent`, `icc-profile-invalid`,
`unembedded-font`, `restricted-font-license`, `color-space-mismatch`,
`transparency` and `limit-exceeded`.

### Helpers

`pdf-date-string`, `xmp-date-string`, `pdf-text-string`, `md5-hex`
and `build-xmp-packet` are exported for callers that build their own
metadata.

## Testing

The unit tests are in [tests/run.scm](tests/run.scm) and run with
`chicken-install -test`.

[tests/verapdf-check.sh](tests/verapdf-check.sh) writes a set of test
documents and validates each one with veraPDF. Documents named
`pass-*` must pass and documents named `fail-*` must fail. Most of the
documents need an RGB ICC profile (version 2) and a TrueType font whose
licence allows embedding. Name them with environment variables:

```bash
PDFA_ICC_PROFILE=path/to/sRGB.icc PDFA_TTF_FONT=path/to/font.ttf tests/verapdf-check.sh
```

The script skips any document whose files are not given. It finds
`csi` through `$CSI`, then `PATH`, and veraPDF through `$VERAPDF`, then
`PATH`. If it finds no veraPDF, it skips the check. Set `VERBOSE=1` to
print the full veraPDF report for each document. Install the egg before
you run the script.

## Scope

`pdfa` targets PDF/A-1b, which guarantees that a document looks the
same when it is reproduced. It does not produce PDF/A-1a, which also
requires a tagged logical structure, or the later parts of PDF/A.

## License

BSD. Written by Ivan Raikov.
