# `propaganda`

Planet Lisp feed tooter: https://functional.cafe/@lisp

## Dev

Setup in the REPL

```lisp
(ql:quickload '(:dexador :cl-ppcre :feeder :arrows :puri :plump :cl-redis))
(push #p"/Users/alex/dev/lisp/propaganda/" asdf:*central-registry*)
(asdf:load-system "propaganda")
```

## Building

Look at the [Dockerfile](Dockerfile) or if you want to run it locally:

### Via ASDF

```shell
sbcl --eval \
"(ql:quickload '(:dexador :cl-ppcre :feeder :arrows :puri :plump))" \
--eval "(push #p\"/Users/alex/dev/lisp/propaganda/\" asdf:*central-registry*)" \
--eval "(asdf:make :propaganda)" --quit
```

### Via SBCL's `save-lisp-and-die`

For a smaller size of executable, due to compression:

```shell
sbcl --eval "(ql:quickload '(:dexador :cl-ppcre :feeder :arrows :puri :plump))" \
  --eval "(push #p\"/Users/alex/dev/lisp/propaganda/\" asdf:*central-registry*)" \
  --eval "(asdf:load-system :propaganda)" \
  --eval "(sb-ext:save-lisp-and-die \"propaganda\" :executable t :toplevel #'propaganda:main :compression t)"
```

## Deployment

The following env vars are needed:

- `MASTODON_ACCESS_TOKEN`
- `REDIS_HOST`
- `REDIS_PORT`
- `REDIS_PASSWORD`

### Docker

#### Build

    docker build -t propaganda .

#### Create .env

    MASTODON_ACCESS_TOKEN=your_actual_token_here
    REDIS_HOST=redis-host
    REDIS_PORT=redis-port
    REDIS_PASSWORD=redis-password

#### Run

    docker run -d --name propaganda-service \
      --env-file .env \
      propaganda
