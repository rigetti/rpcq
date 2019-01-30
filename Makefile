SBCL_BIN=sbcl
SBCL=$(SBCL_BIN) --noinform --no-userinit --no-sysinit --non-interactive

QUICKLISP_HOME=$(HOME)/quicklisp
QUICKLISP_SETUP=$(QUICKLISP_HOME)/setup.lisp
QUICKLISP=$(SBCL) --load $(QUICKLISP_HOME)/setup.lisp \
	--eval '(push (truename ".") asdf:*central-registry*)'

UNAME_S=$(shell uname -s)

all: build

.PHONY: install-test-deps
install-test-deps:
ifeq ($(UNAME_S),Linux)
ifeq ($(shell sed -n "s/^ID=//p" /etc/os-release),debian)
	apt-get install -y libzmq3-dev
else
	echo "Centos-based platforms unsupported"
endif
else
	echo "Non-Linux-based platforms unsupported"
endif

.PHONY: install-build-deps
install-build-deps: install-test-deps

test:
	$(QUICKLISP) \
		 --eval "(ql:quickload :rpcq-tests)" \
		 --eval '(asdf:test-system :rpcq)'

build:
	$(QUICKLISP) \
		 --eval "(ql:quickload :rpcq)"
