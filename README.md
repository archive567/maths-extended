<h1 align="center">
    <a href="https://github.com/tonyday567/maths-extended">
        Maths-Extended
    </a>
</h1>

<hr>

Maths-Extended is some general maths'y stuff

## Develop

``` sh
$ git clone https://github.com/tonyday567/maths-extended.git
$ cd maths-extended

$ cabal sandbox init
$ cabal configure --enable-tests
$ cabal install --dependencies-only --dry-run
$ cabal install --dependencies-only
$ cabal build
$ cabal test
```
