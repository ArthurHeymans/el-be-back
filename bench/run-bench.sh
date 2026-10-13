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
# to be in a checkout.  Note a .eln placed on load-path is silently ignored
# (only native-comp-eln-load-path is searched, keyed by a hash of the
# compiled file's path), so compile into a scratch eln directory and record
# explicit native-elisp-load forms, in dependency order, for the benchmark
# process to load first.
"$EMACS" --batch -Q \
    --eval "(setq native-comp-eln-load-path (list \"$NATIVE\"))" \
    -L "$ROOT" -L "$EAT" -L "$GHOSTEL/lisp" -L "$GHOSTEL" \
    --eval "(progn
      (require 'native-compile)
      (with-temp-file \"$NATIVE/load-native.el\"
        (dolist (f '(\"$ROOT/ebb-graphics.el\" \"$ROOT/ebb-term.el\"
                     \"$ROOT/ebb-parse.el\" \"$ROOT/ebb-render.el\"
                     \"$EAT/eat.el\" \"$GHOSTEL/lisp/ghostel.el\"))
          (insert (format \"(native-elisp-load %S)\\n\"
                          (native-compile f))))))"

# Not exec: the EXIT trap above must run to remove the scratch eln directory.
"$EMACS" --batch -Q \
    -L "$ROOT" -L "$EAT" -L "$GHOSTEL/lisp" -L "$GHOSTEL" \
    -l "$NATIVE/load-native.el" \
    -l "$ROOT/bench/ebb-bench.el" \
    --eval "(setq ebb-bench-size $SIZE ebb-bench-iterations $ITERATIONS)" \
    --eval '(ebb-bench-run)'
