mkdir -p ~/.emacs.d/config
mkdir -p ~/.emacs.d/secrets
ln -sf `pwd`/init.el ~/.emacs.d/
ln -sf `pwd`/config/general.el ~/.emacs.d/config/
ln -sf `pwd`/config/programming.el ~/.emacs.d/config/
ln -sf `pwd`/secrets/.authinfo.gpg ~/.emacs.d/secrets/
