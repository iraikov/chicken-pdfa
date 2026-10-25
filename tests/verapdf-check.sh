#!/bin/sh
#
# verapdf-check.sh -- validates the pdfa egg's output with veraPDF.
#
# Writes the fixtures from verapdf-fixtures.scm into a temporary
# directory and runs veraPDF (PDF/A-1b profile) on each one. A fixture
# named pass-* must be accepted; one named fail-* must be rejected. The
# script exits non-zero if any fixture gives the wrong result.
#
# veraPDF is found through $VERAPDF, then the PATH; csi through $CSI,
# then the PATH. If veraPDF cannot be found, the script prints a note
# and exits 0, like the guarded tests in run.scm. Set VERBOSE=1 to print
# veraPDF's rule-by-rule report for every fixture.
#
# Most fixtures need an ICC profile and a TrueType font, named by the
# environment variables PDFA_ICC_PROFILE and PDFA_TTF_FONT (see
# verapdf-fixtures.scm). Fixtures whose files are not given are skipped.
#
# Usage: PDFA_ICC_PROFILE=sRGB.icc PDFA_TTF_FONT=font.ttf \
#          tests/verapdf-check.sh
#        (after chicken-install has installed pdfa and pdf-truetype)

set -u

here=$(cd "$(dirname "$0")" && pwd)
csi=${CSI:-csi}

verapdf=${VERAPDF:-}
if [ -z "$verapdf" ]; then
    verapdf=$(command -v verapdf 2>/dev/null || true)
fi
if [ -z "$verapdf" ]; then
    echo "note: veraPDF not found; skipping PDF/A validation"
    exit 0
fi

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

"$csi" -s "$here/verapdf-fixtures.scm" "$work" || {
    echo "error: writing the fixtures failed"
    exit 1
}

status=0
for f in "$work"/*.pdf; do
    [ -e "$f" ] || continue
    name=$(basename "$f" .pdf)
    report=$("$verapdf" --flavour 1b --format text -v "$f" 2>&1)
    if printf '%s\n' "$report" | head -1 | grep -q '^PASS'; then
        result=PASS
    else
        result=FAIL
    fi
    case "$name" in
        pass-*) expected=PASS ;;
        fail-*) expected=FAIL ;;
        *)      expected=PASS ;;
    esac
    if [ "$result" = "$expected" ]; then
        echo "ok    $name (veraPDF: $result)"
    else
        echo "WRONG $name (veraPDF: $result, expected $expected)"
        status=1
    fi
    if [ "$result" != "$expected" ] || [ "${VERBOSE:-0}" = 1 ]; then
        printf '%s\n' "$report" | sed 's/^/      /'
    fi
done

exit $status
