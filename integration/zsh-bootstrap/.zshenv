# Chomp zsh startup bootstrap.
if [[ -n "$CHOMP_ZSH_ZDOTDIR" ]]; then
  export ZDOTDIR="$CHOMP_ZSH_ZDOTDIR"
else
  unset ZDOTDIR
fi
unset CHOMP_ZSH_ZDOTDIR

[[ -r "${ZDOTDIR:-$HOME}/.zshenv" ]] && source "${ZDOTDIR:-$HOME}/.zshenv"
source "$CHOMP_SHELL_INTEGRATION_DIR/zsh"
