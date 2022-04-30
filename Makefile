SHELL=/usr/bin/env bash

EMACS ?= emacs
EASK ?= eask

ci: build compile checkdoc lint

build:
	$(EASK) package
	$(EASK) install

compile:
	@echo "Compiling..."
	$(EASK) compile

checkdoc:
	$(EASK) checkdoc

lint:
	@echo "package linting..."
	$(EASK) lint

clean:
	$(EASK) clean-all

.PHONY : test compile checkdoc lint clean tag
