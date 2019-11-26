{ stdenv, lib, bash, electron_6, writeTextFile }:

let version = "0.5"; in

let restless-lisp-source = stdenv.mkDerivation {
  name = "restless-lisp-source-${version}";
  src = ./.;
  buildPhase = "true";
  installPhase = ''
    mkdir -p "$out/share/restless-lisp"
    cp $src/* "$out/share/restless-lisp"
  '';
}; in

let deps = [electron_6];

in writeTextFile {
  name = "restless-lisp-${version}";
  executable = true;
  destination = "/bin/restless-lisp";
  text = ''
    #!${bash}/bin/bash
    export PATH="${lib.makeBinPath deps}":/run/wrappers/bin
    electron 2>/dev/null ${restless-lisp-source}/share/restless-lisp/electron.js
  '';
}
