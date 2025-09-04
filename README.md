# `propaganda`

Planet Lisp feed tooter: https://functional.cafe/@lisp

## Dev

Setup in the REPL

```lisp
(ql:quickload '(:dexador :cl-ppcre :feeder :arrows :puri :plump))
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
- `DATABASE_PATH`

### Docker

#### Build

    docker build -t propaganda .

#### Create .env

    DATABASE_PATH=/var/lib/propaganda/propaganda.db
    MASTODON_ACCESS_TOKEN=your_actual_token_here

#### Run

    docker run -d --name propaganda-service \
      -v propaganda-data:/var/lib/propaganda \
      --env-file .env \
      propaganda
