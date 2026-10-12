#!/usr/bin/env bash
# Benchmark Ebb alone (parse + screen model + render), end to end.
#
# Unlike run-bench.sh this needs no Eat/Ghostel checkouts, so it can run
# in CI.  Results are written as TSV ("case<TAB>ms-per-iter<TAB>MiB/s")
# to --out FILE (default: stdout only, human-readable) for consumption
# by bench/compare.py, which gates pull requests on regressions
# against their base branch measured on the same machine.
#
# By default the Ebb sources are byte-compiled into a temporary
# directory first: fast, and available on every Emacs.  Set
# EBB_BENCH_NATIVE=1 to native-compile instead (closer to how users
# run Ebb, but much slower to set up).  Either way both sides of a
# comparison must use the same mode.
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
EMACS="${EMACS:-emacs}"
SIZE="${SIZE:-262144}"
ITERATIONS="${ITERATIONS:-5}"
OUT=""

usage() {
    cat <<EOF
Usage: $(basename "$0") [--size BYTES] [--iterations N] [--out FILE]

EMACS and EBB_BENCH_NATIVE may be set in the environment.
EOF
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --size) SIZE="$2"; shift 2 ;;
        --iterations) ITERATIONS="$2"; shift 2 ;;
        --out) OUT="$2"; shift 2 ;;
        -h|--help) usage; exit 0 ;;
        *) echo "unknown option: $1" >&2; usage >&2; exit 2 ;;
    esac
done

COMPILED="$(mktemp -d "${TMPDIR:-/tmp}/ebb-bench.XXXXXX")"
trap 'rm -rf "$COMPILED"' EXIT

if [[ "${EBB_BENCH_NATIVE:-}" == "1" ]]; then
    # A .eln placed on load-path is silently ignored (only
    # native-comp-eln-load-path is searched, keyed by a hash of the
    # compiled file's path).  Compile into a scratch eln directory
    # and record explicit native-elisp-load forms, in dependency
    # order, for the benchmark process to load first.
    "$EMACS" --batch -Q \
        --eval "(setq native-comp-eln-load-path (list \"$COMPILED\"))" \
        -L "$ROOT" --eval "(progn
      (require 'native-compile)
      (with-temp-file \"$COMPILED/load-native.el\"
        (dolist (f '(\"ebb-graphics\" \"ebb-term\" \"ebb-parse\" \"ebb-render\"))
          (insert (format \"(native-elisp-load %S)\\n\"
                          (native-compile
                           (expand-file-name (concat f \".el\") \"$ROOT\")))))))"
else
    "$EMACS" --batch -Q -L "$COMPILED" -L "$ROOT" --eval "(progn
      (require 'bytecomp)
      (dolist (f '(\"ebb-graphics\" \"ebb-term\" \"ebb-parse\" \"ebb-render\"))
        (let ((byte-compile-dest-file-function
               (lambda (_) (expand-file-name (concat f \".elc\") \"$COMPILED\"))))
          (byte-compile-file (expand-file-name (concat f \".el\") \"$ROOT\")))))"
fi

ARGS=(--batch -Q)
if [[ "${EBB_BENCH_NATIVE:-}" == "1" ]]; then
    # Load the .eln files before the benchmark's requires see them as
    # already-provided features.
    ARGS+=(-l "$COMPILED/load-native.el")
fi
ARGS+=(-L "$COMPILED" -L "$ROOT" -l "$ROOT/bench/ebb-bench.el"
      --eval "(setq ebb-bench-size $SIZE ebb-bench-iterations $ITERATIONS)")
if [[ -n "$OUT" ]]; then
    ARGS+=(--eval "(setq ebb-bench-output-file \"$OUT\")")
fi
ARGS+=(--eval "(ebb-bench-run 'ebb)")

# Not exec: the EXIT trap above must run to remove the scratch
# compilation directory.
"$EMACS" "${ARGS[@]}"
