#!/usr/bin/env fish
# el-be-back auto-injection for fish
# Fish auto-loads vendor_conf.d files from XDG_DATA_DIRS.

# Restore XDG_DATA_DIRS
if set -q EBB_SHELL_INTEGRATION_XDG_DIR
    set -gx XDG_DATA_DIRS (string replace -- "$EBB_SHELL_INTEGRATION_XDG_DIR:" "" "$XDG_DATA_DIRS")
    set -e EBB_SHELL_INTEGRATION_XDG_DIR
end

# Source ebb integration
if set -q EMACS_EBB_PATH
    source "$EMACS_EBB_PATH/etc/ebb.fish"
end
