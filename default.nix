{ mkDerivation, aeson, array, base, base64-bytestring, binary
, blaze-html, blaze-markup, bytestring, Cabal, cmark, containers
, data-default, deepseq, directory, extensible-exceptions
, filemanip, filepath, ghc-prim, ghcjs-base, ghcjs-dom, ghcjs-prim
, hslua, HTTP, mtl, network, network-uri, old-time, pandoc-types
, parsec, process, random, SHA, stdenv, syb, tagsoup, temporary
, text, time, transformers, unordered-containers, vector, xml, yaml
, zip-archive, zlib
}:
mkDerivation {
  pname = "pandoc-light";
  version = "1.17.2";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson array base base64-bytestring binary blaze-html blaze-markup
    bytestring cmark containers data-default deepseq directory
    extensible-exceptions filemanip filepath ghc-prim hslua HTTP mtl
    network network-uri old-time pandoc-types parsec process random SHA
    syb tagsoup temporary text time unordered-containers vector xml
    yaml zip-archive zlib
  ];
  executableHaskellDepends = [
    base blaze-html Cabal ghcjs-base ghcjs-dom ghcjs-prim text
    transformers
  ];
  homepage = "http://pandoc.org";
  description = "Conversion between markup formats";
  license = "GPL";
}
