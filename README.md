# yesod-sqlite-demo

[![docs](https://github.com/artemmavrin/yesod-sqlite-demo/actions/workflows/docs.yml/badge.svg)](https://artemmavrin.github.io/yesod-sqlite-demo)

This is a simple application built to learn the [Yesod](https://www.yesodweb.com/) Haskell web framework. It uses [SQLite](https://www.sqlite.org/index.html) and [yesod-persistent](https://hackage.haskell.org/package/yesod-persistent) to provide a persistent storage layer.
An even simpler Yesod web application is available at [artemmavrin/yesod-hello-world](https://github.com/artemmavrin/yesod-hello-world)

The application itself is a simple floating point calculator that supports variables, which are stored in a SQLite database (the *environment*).

## Development

This project uses [`stack`](https://docs.haskellstack.org/en/stable/) for building, running, and documentation.

### Run Server

Start the server locally at [`http://localhost:3000`](http://localhost:3000) using
```text
stack run
```

## Examples

We start with a fresh environment:
```bash
$ curl http://localhost:3000/variables
[]
```
Next, we assign `a = 10` and `b = -0.5`,
```bash
$ curl http://localhost:3000/variables -H 'Content-Type: application/json' -d '{"a": 10, "b": -0.5}
```
and query the variables again
```bash
$ curl http://localhost:3000/variables
[{"created":"2022-11-29T07:15:39.591669Z","name":"a","updated":"2022-11-29T07:15:39.591669Z","value":10},{"created":"2022-11-29T07:15:39.59253Z","name":"b","updated":"2022-11-29T07:15:39.59253Z","value":-0.5}]
```
We can query individual variable values using either
```bash
$ curl 'http://localhost:3000/variables?name=a'
[{"created":"2022-11-29T07:15:39.591669Z","name":"a","updated":"2022-11-29T07:15:39.591669Z","value":10}]
```
or
```bash
$ curl http://localhost:3000/variables/a
{"created":"2022-11-29T07:15:39.591669Z","name":"a","updated":"2022-11-29T07:15:39.591669Z","value":10}
```
Next we update the value of `a` to `100`
```bash
curl -X PUT http://localhost:3000/variables/a -H 'Content-Type: application/json' -d '{"value": 100}'
```
and confirm the update
```bash
$ curl http://localhost:3000/variables/a
{"created":"2022-11-29T07:15:39.591669Z","name":"a","updated":"2022-11-29T07:21:36.719543Z","value":100}
```
Lastly, we evaluate the expression `1+2*(a*b)/(b-3)`
```bash
$ curl -X POST http://localhost:3000 -H 'Content-Type: application/json' -d '{"expr": "1+2*(a*b)/(b-3)"}'
{"value":29.571428571428573}
```
