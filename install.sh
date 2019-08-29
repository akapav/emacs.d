#!/bin/bash

EMACS_CFG_DIR=`dirname "$0"`
echo $EMACS_CFG_DIR
rm -rf ~/.emacs.d
cp -r $EMACS_CFG_DIR ~/.emacs.d
