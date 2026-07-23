# Preserve the user's login-shell profile while Ebb owns ZDOTDIR.
__ebb_user_zdotdir="${EBB_ZSH_USER_ZDOTDIR:-$HOME}"
[[ -r "$__ebb_user_zdotdir/.zprofile" ]] && source "$__ebb_user_zdotdir/.zprofile"
unset __ebb_user_zdotdir
