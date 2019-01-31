FROM rigetti/quicklisp

# install build dependencies
COPY Makefile /src/rpcq/Makefile
WORKDIR /src/rpcq
RUN make install-build-deps

# build rpcq
ADD . /src/rpcq
WORKDIR /src/rpcq
RUN git clean -fdx && make
