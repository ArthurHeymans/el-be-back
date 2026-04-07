#!/usr/bin/env fish
# el-be-back shell integration for fish
# Provides OSC 7 (CWD), OSC 133 (semantic prompts), and OSC 51 (Elisp eval).

# Idempotency guard
if set -q __EBB_INTEGRATION_LOADED
    exit
end
set -g __EBB_INTEGRATION_LOADED 1

# --- OSC 7: Report CWD ---
function __ebb_osc7
    printf '\e]7;file://%s%s\e\\' (hostname) (string escape --style=url -- $PWD)
end

# --- OSC 133: Semantic prompt markers ---
function __ebb_prompt --on-event fish_prompt
    set -l exit_status $status
    # D: finish previous command
    printf '\e]133;D;%d\e\\' $exit_status
    # A: prompt start
    printf '\e]133;A\e\\'
    # Report CWD
    __ebb_osc7
end

function __ebb_preexec --on-event fish_preexec
    # B: command start (after prompt, before user types)
    printf '\e]133;B\e\\'
    # C: output start
    printf '\e]133;C\e\\'
end

function __ebb_postexec --on-event fish_postexec
    # Nothing extra needed; D is emitted at next prompt
end

# --- OSC 51: Elisp eval helper ---
function ebb_cmd
    set -l payload "\"$argv[1]\""
    for arg in $argv[2..]
        set arg (string replace --all '\\' '\\\\' -- $arg)
        set arg (string replace --all '"' '\\"' -- $arg)
        set payload "$payload \"$arg\""
    end
    printf '\e]51;E%s\e\\' $payload
end
