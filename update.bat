@echo off
cd ~/git/emacs-config
git pull
emacs -Q ~/git/emacs-config/init.el.org --batch --eval="(org-babel-tangle)"
