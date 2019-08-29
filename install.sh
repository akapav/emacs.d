#!/bin/bash
EMACS_CFG_DIR=`dirname "$0"`

echo "THIS WILL DELETE ~/.emacs.d"
read -p "ARE YOU SURE (type YES)? " choice
case "$choice" in
  YES ) echo "copying $EMACS_CFG_DIR -> ~/.emacs.d";;
  * ) echo "leaving ...";;
esac

rm -rf ~/.emacs.d
cp -r $EMACS_CFG_DIR ~/.emacs.d
