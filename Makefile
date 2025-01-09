.phony: clean start debug prepare
start: prepare
	emacs --init-directory=.

debug: prepare
	emacs --debug-init --init-directory=.

clean:
	rm -fr emacs.el eln-cache site-lisp elpa \
	customizations.el \
	.emacs.desktop \
	auto-save-list \
	etc var

prepare: site-lisp/compat site-lisp/no-littering

site-lisp/compat:
	git clone https://github.com/emacs-compat/compat site-lisp/compat

site-lisp/no-littering:
	git clone https://github.com/emacscollective/no-littering site-lisp/no-littering
