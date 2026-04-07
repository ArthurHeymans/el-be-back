#!/usr/bin/env zsh
# el-be-back shell integration for zsh
# Provides OSC 7 (CWD), OSC 133 (semantic prompts), and OSC 51 (Elisp eval).

# Idempotency guard
[[ -n "$__EBB_INTEGRATION_LOADED" ]] && return
__EBB_INTEGRATION_LOADED=1

# --- OSC 7: Report CWD ---
__ebb_osc7() {
    printf '\e]7;file://%s%s\e\\' "$(hostname)" "$PWD"
}

# --- OSC 133: Semantic prompt markers ---
__ebb_precmd() {
    local exit_status=$?
    # D: finish previous command (with exit status)
    printf '\e]133;D;%d\e\\' "$exit_status"
    # A: prompt start
    printf '\e]133;A\e\\'
    # Report CWD
    __ebb_osc7
}

__ebb_preexec() {
    # C: output start (command is about to run)
    printf '\e]133;C\e\\'
}

# Install hooks (append to arrays)
precmd_functions+=(__ebb_precmd)
preexec_functions+=(__ebb_preexec)

# B marker: emitted after prompt rendering, before user input
# Use zle-line-init hook or PS1 prefix
PS1=$'%{\e]133;B\e\\%}'"$PS1"

# --- OSC 51: Elisp eval helper ---
ebb_cmd() {
    local cmd="$1"
    shift
    local payload="\"$cmd\""
    for arg in "$@"; do
        arg="${arg//\\/\\\\}"
        arg="${arg//\"/\\\"}"
        payload="$payload \"$arg\""
    done
    printf '\e]51;E%s\e\\' "$payload"
}
