MODE_NAME=org-sync
VERSION=$$(grep "^;; Version: " $(MODE_NAME).el | cut -f3 -d' ')
PACKAGE_FOLDER=$(MODE_NAME)-$(VERSION)
ARCHIVE=$(PACKAGE_FOLDER).tar
EMACS=emacs

.PHONY: clean

deps:
	cask

build:
	cask build

clean-dist:
	rm -rf dist/

clean: clean-dist
	rm -rf *.tar
	cask clean-elc

install:
	cask install

test: clean
	cask exec ert-runner

pkg-el:
	cask package

package: clean pkg-el
	cp dist/$(ARCHIVE) .
	make clean-dist

info:
	cask info
