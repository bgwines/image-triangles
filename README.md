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
- [x] Confirm diagrams is rendering triangles in the correct places.
- [x] Cache transformations to the colors library
- [x] Hip has a map transformation. It also depends on the colours library, does it use it?
- [x] In addition, hip has interfaces to arrays that support operations like map
- [x] Check that hip colors are srgb
- [ ] Think about opacity. What if everything was completely opaque? What should we do with areas that aren’t 100% covered at the end?
- [ ] The diagram needs a final bounding box that's the size of the picture that it comes from.
