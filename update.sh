#!/usr/bin/env bash

cd ~/git/emacs-config
git pull
emacs -Q ~/git/emacs-config/init.el.org --batch --eval="(org-babel-tangle)"
