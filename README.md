# MC
Master of Ceremonies

Usage

```
mc -k a -v "espeak-ng \"hello\"" \
   -k b -v "espeak-ng \"goodbye\"" \
   -k c -v "tonwav 420 | aplay"
```

Install

```
cabal install mc
```

Development

```
cabal build
cabal run mc -- --help
```
