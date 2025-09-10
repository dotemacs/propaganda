# Build stage - Alpine
FROM alpine:3.22.1 AS builder

ARG SBCL_VERSION=2.5.8

RUN apk add --no-cache \
    build-base \
    linux-headers \
    zlib-dev \
    zstd-dev \
    curl \
    ca-certificates \
    sbcl

WORKDIR /app

# Build SBCL from source with zstd support
RUN curl -L -o sbcl.tar.gz https://github.com/sbcl/sbcl/archive/refs/tags/sbcl-${SBCL_VERSION}.tar.gz && \
    tar xzf sbcl.tar.gz && \
    rm sbcl.tar.gz && \
    cd sbcl-sbcl-${SBCL_VERSION} && \
    echo '"${SBCL_VERSION}"' > version.lisp-expr && \
    sh make.sh --with-sb-core-compression --with-sb-xref-for-internals && \
    INSTALL_ROOT=/usr/local sh install.sh && \
    cd .. && rm -rf sbcl-sbcl-${SBCL_VERSION}

# Install Quicklisp
RUN curl -O https://beta.quicklisp.org/quicklisp.lisp && \
    /usr/local/bin/sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)' --eval '(ql:add-to-init-file)' --quit

COPY . .
RUN /usr/local/bin/sbcl --eval "(load \"~/quicklisp/setup.lisp\")" \
    --eval "(ql:quickload '(:dexador :cl-ppcre :feeder :arrows :puri :plump :cl-redis))" \
    --eval "(push #p\"/app/\" asdf:*central-registry*)" \
    --eval "(asdf:load-system :propaganda)" \
    --eval "(sb-ext:save-lisp-and-die \"propaganda\" :executable t :toplevel #'propaganda:main :compression t)" \
    --quit && \
    apk del build-base linux-headers zlib-dev zstd-dev curl && \
    rm -rf ~/quicklisp ~/.cache /usr/local/lib/sbcl /usr/local/bin/sbcl *.lisp *.asd 2>/dev/null || true

# Runtime stage - minimal Alpine
FROM alpine:3.22.1
RUN apk add --no-cache ca-certificates zstd && \
    adduser -D -s /bin/false propaganda
COPY --from=builder /app/propaganda /propaganda
RUN chmod +x /propaganda && chown propaganda:propaganda /propaganda
USER propaganda
CMD ["/bin/sh", "-c", "while true; do /propaganda; sleep 3600; done"]
