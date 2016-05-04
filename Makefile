.PHONY: build doc test install uninstall reinstall clean init-doc gh-pages \
	release pr 9p

NAME = protocol-9p
MOD  = protocol_9p
UNIX_MOD = protocol_9p_unix
MODULES = protocol_9p protocol_9p_s protocol_9p_request protocol_9p_error \
          protocol_9p_response protocol_9p_types protocol_9p_client \
          protocol_9p_server protocol_9p_buffered9PReader \
          protocol_9p_filesystem protocol_9p_infix protocol_9p_info
UNIX_MODULES = flow_lwt_unix client9p_unix server9p_unix lofs9p

WIN_MOD      = protocol_9p_win
WIN_MODULES  = client9p_win

WITH_UNIX=$(shell ocamlfind query unix > /dev/null 2>&1 ; echo $$?)
WITH_TERM=$(shell ocamlfind query lambda-term > /dev/null 2>&1 ; echo $$?)
WITH_PIPE=$(shell ocamlfind query named-pipe.lwt > /dev/null 2>&1 ; echo $$?)

OCAMLBUILD = ocamlbuild -use-ocamlfind #-classic-display
TARGETS = .cma .cmxa
TYPES = .mli .cmi .cmti

PRODUCTS := $(addprefix $(MOD),$(TARGETS))

INSTALL := $(foreach module,$(MODULES),$(addprefix $(module),$(TYPES)))

INSTALL := $(INSTALL) $(MOD).a $(PRODUCTS)
INSTALL := $(addprefix _build/lib/,$(INSTALL))

ifeq ($(WITH_UNIX), 0)
UNIX_PRODUCTS := $(addprefix $(UNIX_MOD),$(TARGETS))
UNIX_INSTALL:=$(foreach module,$(UNIX_MODULES),$(addprefix $(module),$(TYPES)))
UNIX_INSTALL := $(UNIX_INSTALL) $(UNIX_MOD).a $(UNIX_PRODUCTS)
UNIX_INSTALL := $(addprefix _build/unix/,$(UNIX_INSTALL))
PRODUCTS+=$(UNIX_PRODUCTS)
INSTALL+=$(UNIX_INSTALL)
endif

ifeq ($(WITH_TERM), 0)
PRODUCTS+=main.native
endif

ifeq ($(WITH_PIPE), 0)
WIN_PRODUCTS := $(addprefix $(WIN_MOD),$(TARGETS))
WIN_INSTALL:=$(foreach module,$(WIN_MODULES),$(addprefix $(module),$(TYPES)))
WIN_INSTALL := $(UNIX_INSTALL) $(WIN_MOD).a $(WIN_PRODUCTS)
WIN_INSTALL := $(addprefix _build/unix/,$(WIN_INSTALL))
PRODUCTS+=$(WIN_PRODUCTS)
INSTALL+=$(WIN_INSTALL)
endif

build:
	$(OCAMLBUILD) $(PRODUCTS)

doc:
	$(OCAMLBUILD) lib/protocol_9p.docdir/index.html

test: build
	$(OCAMLBUILD) tests.native
	$(OCAMLBUILD) lofs_test.native
	./tests.native
	./lofs_test.native

install:
	ocamlfind install $(NAME) META $(INSTALL)

uninstall:
	ocamlfind remove $(NAME)

reinstall: uninstall install

clean:
	ocamlbuild -clean

init-doc:
	mkdir -p gh-pages
	cd gh-pages && ( \
	  git init && \
	  git remote add origin git@github.com:mirage/ocaml-9p.git && \
	  git fetch && \
	  git checkout gh-pages && \
	  git pull)

gh-pages: doc
	rm -f gh-pages/*.html
	cd gh-pages && cp ../protocol_9p.docdir/*.html .
	cd gh-pages && git add * && git commit -a -m "Update docs"
	cd gh-pages && git push

VERSION = $(shell grep 'version = ' _oasis | sed 's/version = "(.*)"/\1/')
ARCHIVE = https://github.com/mirage/ocaml-9p/archive/$(VERSION).tar.gz

release:
	git tag -a $(VERSION) -m "Version $(VERSION)."
	git push upstream $(VERSION)
	$(MAKE) pr

pr:
	opam publish prepare $(NAME).$(VERSION) $(ARCHIVE)
	OPAMPUBLISHBYPASSCHECKS=1 OPAMYES=1 opam publish \
	submit $(NAME).$(VERSION) && rm -rf $(NAME).$(VERSION)
