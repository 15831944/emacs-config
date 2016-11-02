@echo off
cd %AppData%/git/emacs-config
git pull
emacs -Q ~/git/emacs-config/init.el.org --batch --eval="(org-babel-tangle)"
