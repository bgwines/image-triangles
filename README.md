# image-triangles

make sure you have nix installed
```
curl https://nixos.org/nix/install | sh
```

### to build:
```
nix-build

```
run with
```
./result/bin/image-triangles -o output.svg
```

### to develop on:
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

### todo
    * Why is the area wrong for the top right corner?
    * Rasterization for triangles
    * Cmdline interface that lets you set number of triangles, and smallness
    * Pointy triangle filter
    * Confirm diagrams is rendering triangles in the correct places.
    * Cache transformations to the colors library
    * Hip has a map transformation. It also depends on the colours library, does it use it?
    * Check that hip colors are srgb
    * Think about opacity. What if everything was completely opaque? What should we do with areas that aren’t 100% covered at the end?
  
