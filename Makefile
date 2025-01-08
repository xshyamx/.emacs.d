.phony: clean start debug
start:
	emacs --init-directory=.

debug:
	emacs --debug-init --init-directory=.

clean:
	rm -fr emacs.el eln-cache site-lisp elpa \
	customizations.el \
	.emacs.desktop \
	auto-save-list \
	etc var
