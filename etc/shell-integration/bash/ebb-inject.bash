#!/usr/bin/env bash
# el-be-back auto-injection for bash
# This script is set as ENV to be sourced automatically.
# It exits POSIX mode, sources the user's startup files, then loads
# ebb shell integration.

# Exit POSIX mode so normal bash features work
set +o posix

# Restore original ENV if saved
if [[ -n "$EBB_BASH_ENV" ]]; then
    export ENV="$EBB_BASH_ENV"
    unset EBB_BASH_ENV
else
    unset ENV
fi

# Source user startup files
if [[ -f /etc/profile ]]; then
    source /etc/profile
fi

if [[ -f "$HOME/.bash_profile" ]]; then
    source "$HOME/.bash_profile"
elif [[ -f "$HOME/.bash_login" ]]; then
    source "$HOME/.bash_login"
elif [[ -f "$HOME/.profile" ]]; then
    source "$HOME/.profile"
fi

if [[ -f "$HOME/.bashrc" ]]; then
    source "$HOME/.bashrc"
fi

# Unexport HISTFILE if we set it
if [[ -n "$EBB_BASH_UNEXPORT_HISTFILE" ]]; then
    export -n HISTFILE
    unset EBB_BASH_UNEXPORT_HISTFILE
fi

# Source ebb integration
if [[ -n "$EMACS_EBB_PATH" ]]; then
    source "$EMACS_EBB_PATH/etc/ebb.bash"
fi
