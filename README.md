# `beam-mysql`

## Running tests

```bash
$ docker build . --tag beam-mysql
$ docker run -p 3306:3306 -d -t beam-mysql
$ euler dev
$ cabal --flags="+with-db" run nullable
```
