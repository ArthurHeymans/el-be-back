#!/usr/bin/env bash
# ebb-bench — benchmark ebb vs ghostel vs vterm vs eat vs term
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
EBB_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
EMACS="${EMACS:-emacs}"

# Defaults
MODE="all"
SIZE=""
ITERS=""
INCLUDE_GHOSTEL="t"
INCLUDE_VTERM="t"
INCLUDE_EAT="t"
INCLUDE_TERM="t"
OUTPUT=""

# Try to find backends
GHOSTEL_DIR=""
VTERM_DIR=""
EAT_DIR=""

for dir in "$EBB_DIR/../ghostel" \
           "$HOME/.emacs.d/lib/ghostel" \
           "$HOME/.emacs.d/straight/build/ghostel"; do
    if [ -f "$dir/ghostel.el" ] 2>/dev/null; then
        GHOSTEL_DIR="$(cd "$dir" && pwd)"
        break
    fi
done

for dir in "$EBB_DIR/../vterm" \
           "$HOME/.emacs.d/lib/vterm" \
           "$HOME/.emacs.d/elpa/vterm"*/ \
           "$HOME/.emacs.d/straight/build/vterm"; do
    if [ -f "$dir/vterm.el" ] 2>/dev/null; then
        VTERM_DIR="$(cd "$dir" && pwd)"
        break
    fi
done

for dir in "$EBB_DIR/../eat" \
           "$HOME/.emacs.d/lib/eat" \
           "$HOME/.emacs.d/elpa/eat"*/ \
           "$HOME/.emacs.d/straight/build/eat"; do
    if [ -f "$dir/eat.el" ] 2>/dev/null; then
        EAT_DIR="$(cd "$dir" && pwd)"
        break
    fi
done

usage() {
    cat <<EOF
Usage: $(basename "$0") [OPTIONS]

Benchmark ebb (el-be-back) against other Emacs terminal emulators.

Options:
  --quick          Quick run (100KB data, 2 iterations, single size)
  --no-ghostel     Skip ghostel benchmarks
  --no-vterm       Skip vterm benchmarks
  --no-eat         Skip eat benchmarks
  --no-term        Skip Emacs built-in term benchmarks
  --output FILE    Tee output to FILE
  --size N         Data size in bytes (default: 1048576)
  --iterations N   Override iteration count (default: 3)
  --ghostel-dir D  Path to ghostel package directory
  --vterm-dir DIR  Path to vterm package directory
  --eat-dir DIR    Path to eat package directory
  -h, --help       Show this help

Examples:
  $(basename "$0")                 # Full benchmark (ebb vs all available)
  $(basename "$0") --quick         # Quick sanity check
  $(basename "$0") --no-vterm      # Skip vterm
EOF
    exit 0
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --quick)       MODE="quick"; shift ;;
        --no-ghostel)  INCLUDE_GHOSTEL="nil"; shift ;;
        --no-vterm)    INCLUDE_VTERM="nil"; shift ;;
        --no-eat)      INCLUDE_EAT="nil"; shift ;;
        --no-term)     INCLUDE_TERM="nil"; shift ;;
        --output)      OUTPUT="$2"; shift 2 ;;
        --size)        SIZE="$2"; shift 2 ;;
        --iterations)  ITERS="$2"; shift 2 ;;
        --ghostel-dir) GHOSTEL_DIR="$2"; shift 2 ;;
        --vterm-dir)   VTERM_DIR="$2"; shift 2 ;;
        --eat-dir)     EAT_DIR="$2"; shift 2 ;;
        -h|--help)     usage ;;
        *)             echo "Unknown option: $1"; usage ;;
    esac
done

# Verify ebb module exists
MODULE=""
for ext in dylib so; do
    if [ -f "$EBB_DIR/ebb-module.$ext" ]; then
        MODULE="$EBB_DIR/ebb-module.$ext"
        break
    fi
done
if [ -z "$MODULE" ]; then
    echo "ERROR: ebb native module not found. Run ./build.sh first."
    exit 1
fi
echo "ebb module: $MODULE"

# Verify ghostel
if [ "$INCLUDE_GHOSTEL" = "t" ]; then
    if [ -z "$GHOSTEL_DIR" ]; then
        echo "WARNING: ghostel not found, skipping (use --ghostel-dir to specify)"
        INCLUDE_GHOSTEL="nil"
    else
        echo "ghostel: $GHOSTEL_DIR"
    fi
fi

# Verify vterm
if [ "$INCLUDE_VTERM" = "t" ]; then
    if [ -z "$VTERM_DIR" ]; then
        echo "WARNING: vterm not found, skipping (use --vterm-dir to specify)"
        INCLUDE_VTERM="nil"
    else
        echo "vterm: $VTERM_DIR"
    fi
fi

# Verify eat
if [ "$INCLUDE_EAT" = "t" ]; then
    if [ -z "$EAT_DIR" ]; then
        echo "WARNING: eat not found, skipping (use --eat-dir to specify)"
        INCLUDE_EAT="nil"
    else
        echo "eat: $EAT_DIR"
    fi
fi

# Build load-path
LOAD_PATH="-L $EBB_DIR"
[ "$INCLUDE_GHOSTEL" = "t" ] && LOAD_PATH="$LOAD_PATH -L $GHOSTEL_DIR"
[ "$INCLUDE_VTERM" = "t" ] && LOAD_PATH="$LOAD_PATH -L $VTERM_DIR"
[ "$INCLUDE_EAT" = "t" ] && LOAD_PATH="$LOAD_PATH -L $EAT_DIR"

# Build eval expression
EVAL="(progn"
[ -n "$SIZE" ] && EVAL="$EVAL (setq ebb-bench-data-size $SIZE)"
[ -n "$ITERS" ] && EVAL="$EVAL (setq ebb-bench-iterations $ITERS)"
EVAL="$EVAL (setq ebb-bench-include-ghostel $INCLUDE_GHOSTEL)"
EVAL="$EVAL (setq ebb-bench-include-vterm $INCLUDE_VTERM)"
EVAL="$EVAL (setq ebb-bench-include-eat $INCLUDE_EAT)"
EVAL="$EVAL (setq ebb-bench-include-term $INCLUDE_TERM)"
if [ "$MODE" = "quick" ]; then
    EVAL="$EVAL (ebb-bench-run-quick))"
else
    EVAL="$EVAL (ebb-bench-run-all))"
fi

echo ""

# Run benchmarks
CMD="$EMACS --batch -Q $LOAD_PATH -l $SCRIPT_DIR/ebb-bench.el --eval '$EVAL'"

if [ -n "$OUTPUT" ]; then
    eval "$CMD" 2>&1 | tee "$OUTPUT"
else
    eval "$CMD" 2>&1
fi
