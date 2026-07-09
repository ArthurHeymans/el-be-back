# Load the user's prompt first, then wrap it with Chomp markers.
__chomp_user_zdotdir="${CHOMP_ZSH_USER_ZDOTDIR:-$HOME}"
[[ -r "$__chomp_user_zdotdir/.zshrc" ]] && source "$__chomp_user_zdotdir/.zshrc"
source "$CHOMP_SHELL_INTEGRATION_DIR/zsh"

if [[ "$CHOMP_ZSH_RESTORE_ZDOTDIR" == 1 ]]; then
  export ZDOTDIR="$CHOMP_ZSH_USER_ZDOTDIR"
else
  unset ZDOTDIR
fi
unset CHOMP_ZSH_USER_ZDOTDIR CHOMP_ZSH_RESTORE_ZDOTDIR __chomp_user_zdotdir
