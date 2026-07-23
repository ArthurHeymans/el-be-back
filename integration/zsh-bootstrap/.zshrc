# Load the user's prompt first, then wrap it with Ebb markers.
__ebb_user_zdotdir="${EBB_ZSH_USER_ZDOTDIR:-$HOME}"
[[ -r "$__ebb_user_zdotdir/.zshrc" ]] && source "$__ebb_user_zdotdir/.zshrc"
source "$EBB_SHELL_INTEGRATION_DIR/zsh"

if [[ "$EBB_ZSH_RESTORE_ZDOTDIR" == 1 ]]; then
  export ZDOTDIR="$EBB_ZSH_USER_ZDOTDIR"
else
  unset ZDOTDIR
fi
unset EBB_ZSH_USER_ZDOTDIR EBB_ZSH_RESTORE_ZDOTDIR __ebb_user_zdotdir
