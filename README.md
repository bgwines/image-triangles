# image-triangles

make sure you have nix installed
```
curl https://nixos.org/nix/install | sh
```

to build:
```
nix-build

```
run with
```
./result/bin/image-triangles -o output.svg
```

to develop on:
```
cabal --enable-nix build
```
or
```
echo "nix: True" >> ~/.cabal/config
cabal build
```

run with
```
./dist/build/image-triangles/image-triangles -o output.svg
```
