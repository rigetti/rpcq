rpcq
====

[![Build Status](https://semaphoreci.com/api/v1/projects/05f5d83c-3639-4160-bebb-014b98d30cf0/2275647/badge.svg)](https://semaphoreci.com/rigetti/rpcq)

The asynchronous RPC client-server framework and message specification for
[Rigetti Quantum Cloud Services (QCS)](https://www.rigetti.com/).

Implements an efficient transport protocol by using [ZeroMQ](http://zeromq.org/) (ZMQ) sockets and
[MessagePack](https://msgpack.org/index.html) (`msgpack`) serialization.

Not intended to be a full-featured replacement for other frameworks like
[gRPC](https://grpc.io/) or [Apache Thrift](https://thrift.apache.org/).

python Installation
-------------------

To install directly from the source, run `pip install -e .` from within the top-level
directory of the `rpcq` repository. To additionally install the requirements for testing,
make sure to run `pip install -r requirements.txt`.

To instead install the latest released verson of `rpcq` from the Python package manager PyPi,
run `pip install rpcq`.

**NOTE**: We strongly encourage users of `rpcq` to install the software within a (Python)
virtual environment (read up on [`virtualenv`](https://github.com/pypa/virtualenv),
[`pyenv`](https://github.com/pyenv/pyenv), or [`conda`](https://github.com/conda/conda)
for more info).

Lisp Installation
-----------------

Installation is easier with QuickLisp. After placing the source for RPCQ within your local
Lisp projects directory (cf. `ql:*local-project-directories*`), run `(ql:quickload :rpcq)`
and QuickLisp will download the necessary Lisp dependencies.

In addition to the Lisp dependencies, RPCQ depends on ZeroMQ.  Be sure to install both the
library *and* its development headers (which are necessary for the Lisp foreign-function
interface to get its bearings).

Using the Client-Server Framework
---------------------------------

The following two code samples (first in Python, then in Lisp) demonstrate how to create a server, add a test handler, and spin it up.

```python
from rpcq import Server

server = Server()

@server.rpc_handler
def test():
    return 'test'

server.run('tcp://*:5555')
```

```lisp
(defun test ()
  "test")

(let ((dt (rpcq:make-dispatch-table)))
  (rpcq:dispatch-table-add-handler dt 'test)
  (rpcq:start-server :dispatch-table dt
                     :listen-addresses '("tcp://*:5555")))
```

In another window, we can (again first in Python, then in Lisp) create a client that points to the same socket, and call the test method.

```python
from rpcq import Client

client = Client('tcp://localhost:5555')

client.call('test')
```

```lisp
(rpcq:with-rpc-client (client "tcp://localhost:5555")
  (rpcq:rpc-call client "test"))
```

In all cases (including interoperating a client/server pair written in different languages), this will return the string `'test'`.

Using the Message Spec
----------------------

The message spec as defined in `src/messages.lisp` (which in turn produces `rpcq/messages.py`)
is meant to be used with the [Rigetti QCS](https://www.rigetti.com/qcs) platform. Therefore,
these messages are used in [`pyquil`](https://github.com/rigetticomputing/pyquil), in order
to allow users to communicate with the Rigetti Quil compiler and quantum processing units (QPUs).
PyQuil provides utilities for users to interact with the QCS API and write programs in
[Quil](https://arxiv.org/abs/1608.03355), the quantum instruction language developed at Rigetti.

Thus, most users will not interact with `rpcq.messages` directly. However, for those interested
in building their own implementation of the QCS API utilities in pyQuil, becoming acquainted
with the client-server framework, the available messages in the message spec, and how they
are used in the `pyquil.api` module would be a good place to start!

Updating the Python Message Bindings
------------------------------------

Currently only Python bindings are available for the message spec, but more language bindings
are in the works. To update the Python message bindings after editing `src/messages.lisp`,
open `rlwrap sbcl` and run:

```lisp
(ql:quickload :rpcq)
(with-open-file (f "rpcq/messages.py" :direction :output :if-exists :supersede)
  (rpcq:python-message-spec f))
```

**NOTE**: Requires pre-installed
[`sbcl`](http://www.sbcl.org/),
[`quicklisp`](https://www.quicklisp.org/beta/), and
(optionally) [`rlwrap`](https://github.com/hanslub42/rlwrap).

Running the Unit Tests
----------------------

The `rpcq` repository is configured with SemaphoreCI to automatically run the Python unit tests.
This can be done locally by running `pytest` from the top-level directory of the repository
(assuming you have installed the test requirements).

There is additionally a very small suite of Lisp tests for `rpcq`. These are not run by
SemaphoreCI, but can be run locally by doing the following from within `rlwrap sbcl`:

```lisp
(ql:quickload :rpcq)
(asdf:test-system :rpcq)
```

There may be some instances of `STYLE-WARNING`, but if the test run successfully,
there should be something near the bottom of the output that looks like:

```
RPCQ-TESTS (Suite)
  TEST-DEFMESSAGE                                                         [ OK ]
```

Authors
-------

Developed at [Rigetti Computing](https://github.com/rigetticomputing) by
[Nikolas Tezak](https://github.com/ntezak),
[Steven Heidel](https://github.com/stevenheidel),
[Peter Karalekas](https://github.com/karalekas),
[Eric Peterson](https://github.com/ecp-rigetti),
[Guen Prawiroatmodjo](https://github.com/guenp), and
[Robert Smith](https://github.com/tarballs-are-good).
