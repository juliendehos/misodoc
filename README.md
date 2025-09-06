# misodoc

A Miso app that generates a documentation from MD files (inspired by
[mdBook](https://rust-lang.github.io/mdBook/)).


## TODO

- fetch summary.md + generate links
- detect internal links and convert them to miso actions
- render all mardown node types
- multi-level summary
- prefix/numbered/suffix chapters
- add something to refresh summary
- go to next/previous chapter


## setup

- install Nix Flakes

- install Cachix

- use miso's cachix:

```sh
cachix use haskell-miso-cachix
```


## build and run (wasm)

```
nix develop .#wasm --command bash -c "make && make serve"
```

or (dev):

```
nix develop .#wasm
make build && make serve
```


## build and run (docker)

```
nix develop .#wasm --command bash -c "make"
nix-build docker.nix
docker load < result
docker run --rm -it -p 3000:3000 misodoc:latest
```


## edit with vscode

```
nix-shell
code .
```

