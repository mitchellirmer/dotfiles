# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="powerlevel10k/powerlevel10k"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in $ZSH/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment one of the following lines to change the auto-update behavior
# zstyle ':omz:update' mode disabled  # disable automatic updates
# zstyle ':omz:update' mode auto      # update automatically without asking
# zstyle ':omz:update' mode reminder  # just remind me to update when it's time

# Uncomment the following line to change how often to auto-update (in days).
# zstyle ':omz:update' frequency 13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS="true"

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# You can also set it to another string to have that shown instead of the default red dots.
# e.g. COMPLETION_WAITING_DOTS="%F{yellow}waiting...%f"
# Caution: this setting can cause issues with multiline prompts in zsh < 5.7.1 (see #5765)
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/mitchell/Opt/miniconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/mitchell/Opt/miniconda3/etc/profile.d/conda.sh" ]; then
        . "/home/mitchell/Opt/miniconda3/etc/profile.d/conda.sh"
    else
        export PATH="/home/mitchell/Opt/miniconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

## GPG STUFF
export GPG_TTY=$(tty)

## HOMEBREW STUFF
eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
##

##################################################################
################## CUSTOM ALIASES ################################
#################################################################

## CLI STUFF
alias web='w3m https://www.duckduckgo.com -config $HOME/.w3m/config'
alias linklist='lynx -dump -listonly -nonumbers'
alias websave='lynx -dump -nolist'

## emacs org mode helper scripts
alias syncorgdb='sh $HOME/Documents/Org/helperScripts/syncorgdb.sh'
alias resyncorgdb='sh $HOME/Documents/Org/helperScripts/syncorgdb.sh'

####### CLISUITE INSTALLATION BELOW
alias metube='sh $HOME/.clisuite/src/metube.sh'
alias readmode='sh $HOME/.clisuite/src/readmode.sh'
alias speckify='sh $HOME/.clisuite/src/speckify.sh'
####### END CLISUITE INSTALLATION

## work these next two into speckify
alias mpa='mpv --no-video'
alias mpash='mpv --no-video --shuffle'
##
alias weather='cd /home/mitchell/Opt/weather-2.4; ./weather --forecast mry; cd'

## if I wrote the script this will tell me what it does.
alias reminder='grep ":=" *.sh'

## This next section is also on Minte's macbook -- update hers if I update these
alias manamana="mpv 'https://www.youtube.com/watch?v=9ytei6bu7kQ&pp=ygULbWFobmE$'"
alias popcorn="mpv 'https://www.youtube.com/watch?v=B7UmUX68KtE&pp=ygUUcG9wY29y$'"
alias lpr='mpv --no-video https://lpr.streamguys1.com/lpr-aac'
alias cows="mpv 'https://www.youtube.com/watch?v=Z1f9b7sX_XY&pp=ygUTY293cyBzYW5$'"
alias trex="mpv 'https://www.youtube.com/watch?v=y1nBhDVuTR0&pp=ygUTdHJleCBzYW5$'"
alias chickens="mpv 'https://www.youtube.com/watch?v=0Pm9_M-94XY&pp=ygUVcGhpbGF$'"
alias piggies="mpv 'https://www.youtube.com/watch?v=6x0aUhUJDBE&pp=ygUPcGVyZmVj$'"
alias vidshuffle='mpv --shuffle .vidshuffle.m3u'
alias mitch='sh /home/mitchell/Videos/.mitch.txt'
##

# >>> juliaup initialize >>>

# !! Contents within this block are managed by juliaup !!

path=('/home/mitchell/.juliaup/bin' $path)
export PATH

# <<< juliaup initialize <<<
