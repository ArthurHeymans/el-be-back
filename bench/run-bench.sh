#!/usr/bin/env bash
# Compare Ebb, Eat, and Ghostel with native-compiled Elisp.
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
EAT="${EAT_DIR:-$ROOT/../emacs-eat}"
GHOSTEL="${GHOSTEL_DIR:-$ROOT/../ghostel}"
EMACS="${EMACS:-emacs}"
SIZE="${SIZE:-1048576}"
ITERATIONS="${ITERATIONS:-5}"

usage() {
    cat <<EOF
Usage: $(basename "$0") [--quick] [--size BYTES] [--iterations N]

EAT_DIR, GHOSTEL_DIR, and EMACS may be set in the environment.
The script native-compiles Ebb, Eat, and Ghostel into a temporary directory
before running; this is required for a meaningful comparison.
EOF
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --quick) SIZE=102400; ITERATIONS=2; shift ;;
        --size) SIZE="$2"; shift 2 ;;
        --iterations) ITERATIONS="$2"; shift 2 ;;
        -h|--help) usage; exit 0 ;;
        *) echo "unknown option: $1" >&2; usage >&2; exit 2 ;;
    esac
done

if [[ ! -f "$GHOSTEL/ghostel-module.so" && ! -f "$GHOSTEL/ghostel-module.dylib" ]]; then
    echo "Ghostel native module not found in $GHOSTEL; build it first (zig build -Doptimize=ReleaseFast)." >&2
    exit 1
fi
if [[ ! -f "$EAT/eat.el" ]]; then
    echo "Eat source not found: $EAT" >&2
    exit 1
fi
if ! "$EMACS" --batch -Q --eval '(unless (and (fboundp (quote native-comp-available-p)) (native-comp-available-p)) (kill-emacs 1))'; then
    echo "An Emacs with native compilation is required." >&2
    exit 1
fi

NATIVE="$(mktemp -d "${TMPDIR:-/tmp}/ebb-native.XXXXXX")"
trap 'rm -rf "$NATIVE"' EXIT

# Compile the implementations used by the benchmark.  Loading the resulting
# .eln files is preferable to relying on whatever stale .elc/.eln files happen
# to be in a checkout.
"$EMACS" --batch -Q \
    -L "$ROOT" -L "$EAT" -L "$GHOSTEL/lisp" -L "$GHOSTEL" \
    --eval "(progn
      (require 'native-compile)
      (native-compile \"$ROOT/ebb-term.el\" \"$NATIVE/ebb-term.eln\")
      (native-compile \"$ROOT/ebb-parse.el\" \"$NATIVE/ebb-parse.eln\")
      (native-compile \"$ROOT/ebb-render.el\" \"$NATIVE/ebb-render.eln\")
      (native-compile \"$EAT/eat.el\" \"$NATIVE/eat.eln\")
      (native-compile \"$GHOSTEL/lisp/ghostel.el\" \"$NATIVE/ghostel.eln\"))"

exec "$EMACS" --batch -Q \
    -L "$NATIVE" -L "$ROOT" -L "$EAT" -L "$GHOSTEL/lisp" -L "$GHOSTEL" \
    -l "$ROOT/bench/ebb-bench.el" \
    --eval "(setq ebb-bench-size $SIZE ebb-bench-iterations $ITERATIONS)" \
    --eval '(ebb-bench-run)'
