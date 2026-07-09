# Chomp zsh startup bootstrap.
if [[ "$CHOMP_ZSH_ZDOTDIR_SET" == 1 ]]; then
  export CHOMP_ZSH_USER_ZDOTDIR="$CHOMP_ZSH_ZDOTDIR"
  export CHOMP_ZSH_RESTORE_ZDOTDIR=1
else
  unset CHOMP_ZSH_USER_ZDOTDIR
  export CHOMP_ZSH_RESTORE_ZDOTDIR=0
fi
unset CHOMP_ZSH_ZDOTDIR CHOMP_ZSH_ZDOTDIR_SET

__chomp_user_zdotdir="${CHOMP_ZSH_USER_ZDOTDIR:-$HOME}"
[[ -r "$__chomp_user_zdotdir/.zshenv" ]] && source "$__chomp_user_zdotdir/.zshenv"
unset __chomp_user_zdotdir
