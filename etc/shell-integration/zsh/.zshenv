#!/usr/bin/env zsh
# el-be-back auto-injection for zsh
# This file is loaded because we set ZDOTDIR to this directory.

# Restore original ZDOTDIR
if [[ -n "$EBB_ZSH_ZDOTDIR" ]]; then
    ZDOTDIR="$EBB_ZSH_ZDOTDIR"
    unset EBB_ZSH_ZDOTDIR
else
    ZDOTDIR="$HOME"
fi

# Source user's .zshenv
if [[ -f "$ZDOTDIR/.zshenv" ]]; then
    source "$ZDOTDIR/.zshenv"
fi

# Source ebb integration
if [[ -n "$EMACS_EBB_PATH" ]]; then
    source "$EMACS_EBB_PATH/etc/ebb.zsh"
fi
