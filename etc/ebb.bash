#!/usr/bin/env bash
# el-be-back shell integration for bash
# Provides OSC 7 (CWD), OSC 133 (semantic prompts), and OSC 51 (Elisp eval).
# Source this file from your .bashrc, or use auto-injection.

# Idempotency guard
[[ -n "$__EBB_INTEGRATION_LOADED" ]] && return
__EBB_INTEGRATION_LOADED=1

# Enable PTY echo -- bash's readline buffers its own echo, but
# the terminal needs PTY-level echo for immediate feedback.
stty echo 2>/dev/null

# --- OSC 7: Report CWD ---
__ebb_osc7() {
    printf '\e]7;file://%s%s\e\\' "$(hostname)" "$PWD"
}

# --- OSC 133: Semantic prompt markers ---
__ebb_prompt_start() {
    # D: finish previous command (with exit status)
    printf '\e]133;D;%s\e\\' "$__ebb_last_exit"
    # A: prompt start
    printf '\e]133;A\e\\'
}

__ebb_preexec() {
    # C: output start (command is about to run)
    printf '\e]133;C\e\\'
}

# Save exit status before PROMPT_COMMAND runs
__ebb_save_exit() {
    __ebb_last_exit=$?
}

# Install hooks
__ebb_last_exit=0

# Wrap PROMPT_COMMAND to insert our hooks
if [[ -z "$PROMPT_COMMAND" ]]; then
    PROMPT_COMMAND='__ebb_save_exit; __ebb_prompt_start; __ebb_osc7'
elif [[ "$PROMPT_COMMAND" != *"__ebb_save_exit"* ]]; then
    PROMPT_COMMAND="__ebb_save_exit; __ebb_prompt_start; __ebb_osc7; $PROMPT_COMMAND"
fi

# B marker: after prompt, before command input
# We use PS1 wrapper to emit B at the end of the prompt
__ebb_original_ps1="$PS1"
PS1='\[\e]133;B\e\\\]'"$PS1"

# Install DEBUG trap for preexec (command start -> output start marker)
if [[ -z "$(trap -p DEBUG)" ]]; then
    trap '__ebb_preexec' DEBUG
fi

# --- OSC 51: Elisp eval helper ---
ebb_cmd() {
    local cmd="$1"
    shift
    local payload="\"$cmd\""
    for arg in "$@"; do
        # Escape double quotes in arguments
        arg="${arg//\\/\\\\}"
        arg="${arg//\"/\\\"}"
        payload="$payload \"$arg\""
    done
    printf '\e]51;E%s\e\\' "$payload"
}
