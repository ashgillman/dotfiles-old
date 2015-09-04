#!/bin/bash
if [[ $UID != 0 ]]; then
    echo "Please run this script with sudo:"
    echo "sudo $0 $*"
    exit 1
fi

# Get ready
#apt-get update -y && apt-get upgrade -y
wget -q -O - https://dl-ssl.google.com/linux/linux_signing_key.pub \
    | sudo apt-key add -
if [ -f /etc/apt/sources.list.d/google-chrome.list ] ; then
    if grep -q 'stable main' /etc/apt/sources.list.d/google-chrome.list
    then
        sh -c 'echo "deb http://dl.google.com/linux/chrome/deb/ stable main" >> /etc/apt/sources.list.d/google-chrome.list'
    fi
fi
apt-key adv --keyserver pgp.mit.edu --recv-keys 5044912E
add-apt-repository \
  "deb http://linux.dropbox.com/ubuntu $(lsb_release -sc) main"

# Install applications
apt-get install -y \
    build-essential \
    build-dep
    curl \
    google-chrome-stable \
    gnome-tweak-tool \
    git \
    nautilus-dropbox \
    python \
    python-pip \
    python3 \
    python3-pip \
    silversearcher-ag \
    vim \
    zsh
apt-get build-dep -y emacs24
command -v docker >/dev/null 2>&1 && echo "docker already installed" || \
    sudo -u $SUDO_USER curl -sSL https://get.docker.com/ | sh

# Dotfile settings
sudo -u $SUDO_USER \
    git clone https://github.com/ashgillman/dotfiles.git ~/proj/dotfiles \
      2>/dev/null || echo "dotfiles already exists."
cd ~/proj/dotfiles
sudo -u $SUDO_USER git submodule init
sudo -u $SUDO_USER git submodule update
sudo -u $SUDO_USER sh link.sh

# Build emacs
EMACS_V="24.5"
sudo -u $SUDO_USER mkdir -p ~/proj
if [ ! -d ~/proj/emacs-$EMACS_V ] ; then
    sudo -u $SUDO_USER curl -L http://ftp.gnu.org/gnu/emacs/emacs-${EMACS_V}.tar.gz | \
    tar -xz -C ~/proj
else
    echo "emacs already downloaded"
fi
cd ~/proj/emacs-$EMACS_V
echo "Need to install Emacs $EMACS_V"
echo "Currently installed $(emacs --version | grep -o [0-9].[.][0-9])" || \
    echo "Emacs not yet installed"
[ $EMACS_V == $(emacs --version | grep -o [0-9].[.][0-9]) ] || \
    ( bash ./configure && make && make install )
cp ~/proj/dotfiles/Emacs.desktop /usr/share/applications/Emacs.desktop
pip install wakatime

# Use zsh
chsh -s /bin/zsh

dropbox autostart y
dropbox start
