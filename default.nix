{ mkDerivation, base, containers, file-embed, ghcjs-dom, lens, mtl
, parsec, reflex, reflex-dom, stdenv, string-qq, text
}:
mkDerivation {
  pname = "fvg";
  version = "0.1.0.0";
  src = ./.;
  configureFlags = [ "-fsite" ];
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers file-embed mtl parsec reflex reflex-dom
  ];
  executableHaskellDepends = [
    base containers file-embed ghcjs-dom lens reflex-dom string-qq text
  ];
  license = stdenv.lib.licenses.bsd3;
}
