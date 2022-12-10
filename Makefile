SHELL=/usr/bin/env bash

EMACS ?= emacs
EASK ?= eask

# TODO: add `lint`
ci: build compile checkdoc

build:
	$(EASK) package
	$(EASK) install

compile:
	@echo "Compiling..."
	$(EASK) compile

checkdoc:
	$(EASK) lint checkdoc

lint:
	@echo "package linting..."
	$(EASK) lint package

clean:
	$(EASK) clean all

.PHONY : test compile checkdoc lint clean tag
