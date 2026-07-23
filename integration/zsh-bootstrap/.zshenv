# Ebb zsh startup bootstrap.
if [[ "$EBB_ZSH_ZDOTDIR_SET" == 1 ]]; then
  export EBB_ZSH_USER_ZDOTDIR="$EBB_ZSH_ZDOTDIR"
  export EBB_ZSH_RESTORE_ZDOTDIR=1
else
  unset EBB_ZSH_USER_ZDOTDIR
  export EBB_ZSH_RESTORE_ZDOTDIR=0
fi
unset EBB_ZSH_ZDOTDIR EBB_ZSH_ZDOTDIR_SET

__ebb_user_zdotdir="${EBB_ZSH_USER_ZDOTDIR:-$HOME}"
[[ -r "$__ebb_user_zdotdir/.zshenv" ]] && source "$__ebb_user_zdotdir/.zshenv"
unset __ebb_user_zdotdir
