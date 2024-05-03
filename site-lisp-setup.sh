#!/bin/sh

clone() {
		src="$1"
		target="$2"
		if [ "$target" == "" ]; then
				target=$(basename $src .git)
		fi
		if [ -e "./$target" ]; then
				printf "\e[32m%s\e[0m already exists\n" $target
		else
				git clone $src $target
		fi
}
download() {
		src="$1"
		target="$2"
		if [ "$target" == "" ]; then
				echo specify target file
				return 1
		fi
		dir=$(dirname $target)
		if [ "$dir" != "" ]; then
				mkdir -p $dir
		fi
		if [ -e "./$target" ]; then
				printf "\e[32m%s\e[0m already exists\n" $target
		else
				curl -o "./$target" -L "$src"
		fi
}
mkdir -p site-lisp
cd site-lisp
download https://raw.githubusercontent.com/emacsmirror/emacswiki.org/master/visual-basic-mode.el visual-basic-mode/visual-basic-mode.el

clone https://github.com/pdorrell/rules-editing-mode.git drools-mode
clone https://github.com/xshyamx/fzf.el.git fzf
clone https://github.com/pashky/restclient.el restclient
clone https://github.com/ccod/dbd-mode dbdiagram-mode

if ! grep "(provide" dbdiagram-mode/dbdiagram-mode.el > /dev/null; then
    echo "(provide 'dbdiagram-mode)" >> dbdiagram-mode/dbdiagram-mode.el
fi

clone https://github.com/xshyamx/gedcom.git gedcom
clone https://github.com/xshyamx/openapi-mode openapi-mode

# git modes
clone https://github.com/rafl/git-commit-mode
clone https://github.com/magit/git-modes
