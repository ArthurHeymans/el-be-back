# Preserve the user's login-shell profile while Chomp owns ZDOTDIR.
__chomp_user_zdotdir="${CHOMP_ZSH_USER_ZDOTDIR:-$HOME}"
[[ -r "$__chomp_user_zdotdir/.zprofile" ]] && source "$__chomp_user_zdotdir/.zprofile"
unset __chomp_user_zdotdir
