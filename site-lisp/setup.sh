#!/bin/sh

mkdir -p visual-basic-mode
curl -o visual-basic-mode/visual-basic-mode.el -L https://raw.githubusercontent.com/emacsmirror/emacswiki.org/master/visual-basic-mode.el
git clone https://github.com/pdorrell/rules-editing-mode.git drools-mode
git clone https://github.com/xshyamx/fzf.el.git fzf
