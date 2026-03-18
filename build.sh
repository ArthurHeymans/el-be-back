#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

echo "Building ebb-module..."
cargo build --release

# Determine library extension and name
case "$(uname -s)" in
    Darwin) lib="libebb_module.dylib" ;;
    *)      lib="libebb_module.so" ;;
esac

src="target/release/${lib}"
dst="ebb-module.so"

if [ ! -f "$src" ]; then
    echo "ERROR: Built library not found at $src" >&2
    exit 1
fi

cp "$src" "$dst"
echo "Installed $dst"
