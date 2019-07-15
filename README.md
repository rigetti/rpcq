rpcq
====

[![pipeline status](https://gitlab.com/rigetti/forest/rpcq/badges/master/pipeline.svg)](https://gitlab.com/rigetti/forest/rpcq/commits/master)
[![pypi version](https://img.shields.io/pypi/v/rpcq.svg)](https://pypi.org/project/rpcq/)
[![conda-forge version](https://img.shields.io/conda/vn/conda-forge/rpcq.svg)](https://anaconda.org/conda-forge/rpcq)
[![docker pulls](https://img.shields.io/docker/pulls/rigetti/rpcq.svg)](https://hub.docker.com/r/rigetti/rpcq)

The asynchronous RPC client-server framework and message specification for
[Rigetti Quantum Cloud Services (QCS)](https://www.rigetti.com/).

Implements an efficient transport protocol by using [ZeroMQ](http://zeromq.org/) (ZMQ) sockets and
[MessagePack](https://msgpack.org/index.html) (`msgpack`) serialization.

Not intended to be a full-featured replacement for other frameworks like
[gRPC](https://grpc.io/) or [Apache Thrift](https://thrift.apache.org/).

Python Installation
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

We can also use the rpcq docker container to update the message spec without to install the
requirements.

```bash
./docker_update_python_spec.sh
```

Running the Unit Tests
----------------------

The `rpcq` repository is configured with GitLab CI to automatically run the unit tests.
The tests run within a container based off of the
[`rigetti/lisp`](https://hub.docker.com/r/rigetti/lisp) Docker image, which is pinned to a specific
tag. If you need a more recent version of the image, update the tag in the `.gitlab-ci.yml`.

The Python unit tests can be executed locally by running `pytest` from the top-level
directory of the repository (assuming you have installed the test requirements).

The Lisp unit tests can be run locally by doing the following from within `rlwrap sbcl`:

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

Automated Packaging with Docker
-------------------------------

The CI pipeline for `rpcq` produces a Docker image, available at
[`rigetti/rpcq`](https://hub.docker.com/r/rigetti/rpcq). To get the latest stable
version of `rpcq`, run `docker pull rigetti/rpcq`. The image is built from the
[`rigetti/lisp`](https://hub.docker.com/r/rigetti/lisp) Docker image, which is pinned to a specific
tag. If you need a more recent version of the image, update the tag in the `Dockerfile`.

To learn more about the `rigetti/lisp` Docker image, check out the
[`docker-lisp`](https://github.com/rigetti/docker-lisp) repository.

Release Process
---------------

1. Update `VERSION.txt` and dependency versions (if applicable) and push the commit to `master`.
2. Push a git tag `vX.Y.Z` that contains the same version number as in `VERSION.txt`.
3. Verify that the resulting build (triggered by pushing the tag) completes successfully.
4. Push the tagged commit to `pypi` and verify it appears [here](https://pypi.org/project/rpcq/).
5. Publish a [release](https://github.com/rigetti/rpcq/releases) using the tag as the name.
6. Close the [milestone](https://github.com/rigetti/rpcq/milestones) associated with this release,
   and migrate incomplete issues to the next one.

Authors
-------

Developed at [Rigetti Computing](https://github.com/rigetticomputing) by
[Nikolas Tezak](https://github.com/ntezak),
[Steven Heidel](https://github.com/stevenheidel),
[Eric Peterson](https://github.com/ecp-rigetti),
[Colm Ryan](https://github.com/caryan),
[Peter Karalekas](https://github.com/karalekas),
[Guen Prawiroatmodjo](https://github.com/guenp), and
[Robert Smith](https://github.com/tarballs-are-good).
