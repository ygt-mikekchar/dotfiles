#!/bin/bash

# Install the latest version of emacs
sudo apt-add-repository ppa:ubuntu-elisp/ppa
sudo apt-get update
sudo apt-get install emacs-snapshot

#Install the latest version of git
sudo add-apt-repository ppa:git-core/ppa
sudo apt-get update
sudo apt-get install git

