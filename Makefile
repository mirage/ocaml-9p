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
PRODUCTS+=src/main.native
endif

build: unix/client9p_unix.ml unix/_tags src/_tags
	$(OCAMLBUILD) $(PRODUCTS)

unix/client9p_unix.ml: unix/client9p_unix.ml.in
ifeq ($(WITH_PIPE), 0)
	cppo -D HAVE_NAMED_PIPE unix/client9p_unix.ml.in > unix/client9p_unix.ml
else
	cppo -U HAVE_NAMED_PIPE unix/client9p_unix.ml.in > unix/client9p_unix.ml
endif

unix/_tags: unix/_tags.in
ifeq ($(WITH_PIPE), 0)
	cp unix/_tags.in unix/_tags
	echo '<*.*>: package(astring), package(named-pipe.lwt)' >> unix/_tags
else
	cp unix/_tags.in unix/_tags
endif


src/_tags: src/_tags.in
ifeq ($(WITH_PIPE), 0)
	cp src/_tags.in src/_tags
	echo '<*.*>: package(astring), package(named-pipe.lwt)' >> src/_tags
else
	cp src/_tags.in src/_tags
endif

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
	rm -f unix/client9p_unix.ml
	rm -f unix/_tags
	rm -f src/_tags

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
