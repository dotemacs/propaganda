# Build stage
FROM debian:bookworm-slim AS builder
ARG SBCL_VERSION=2.5.8

RUN apt-get update && apt-get -y install --no-install-recommends \
    build-essential \
    zlib1g-dev \
    libzstd-dev \
    time \
    curl \
    ca-certificates \
    sbcl \
    && rm -rf /var/lib/apt/lists/*

# Build SBCL from source with zstd support
RUN curl -L -o sbcl.tar.gz https://github.com/sbcl/sbcl/archive/refs/tags/sbcl-${SBCL_VERSION}.tar.gz && \
    tar xzf sbcl.tar.gz && \
    rm sbcl.tar.gz && \
    cd sbcl-sbcl-${SBCL_VERSION} && \
    echo '"${SBCL_VERSION}"' > version.lisp-expr && \
    sh make.sh --with-sb-core-compression --with-sb-xref-for-internals && \
    INSTALL_ROOT=/usr/local sh install.sh && \
    apt-get purge -y sbcl build-essential time && apt-get autoremove -y && \
    cd .. && rm -rf sbcl-sbcl-${SBCL_VERSION}


ENV PATH="/usr/local/bin:$PATH"
RUN mkdir /app
WORKDIR /app

# Install Quicklisp
RUN curl -O https://beta.quicklisp.org/quicklisp.lisp && \
    sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)' --eval '(ql:add-to-init-file)' --quit && \
    apt-get purge -y curl && apt-get autoremove -y

COPY . .
RUN sbcl --eval "(load \"~/quicklisp/setup.lisp\")" \
    --eval "(ql:quickload '(:dexador :cl-ppcre :feeder :arrows :puri :plump :cl-redis))" \
    --eval "(push #p\"/app/\" asdf:*central-registry*)" \
    --eval "(asdf:load-system :propaganda)" \
    --eval "(sb-ext:save-lisp-and-die \"propaganda\" :executable t :toplevel #'propaganda:main :compression t)" \
    --quit && ls -la /app/propaganda && \
    rm -rf ~/quicklisp ~/.cache && \
    apt-get purge -y zlib1g-dev libzstd-dev ca-certificates && \
    apt-get autoremove -y && \
    rm -rf *.lisp *.asd documentation.md test-db.lisp


# Runtime stage
FROM debian:bookworm-slim
RUN apt-get update && apt-get install -y --no-install-recommends libzstd1 && \
    rm -rf /var/lib/apt/lists/* && \
    adduser --disabled-login --gecos "" --shell /bin/false propaganda
COPY --from=builder /app/propaganda /propaganda
RUN chmod +x /propaganda && chown propaganda:propaganda /propaganda
USER propaganda
CMD ["/bin/sh", "-c", "while true; do /propaganda; sleep 3600; done"]
