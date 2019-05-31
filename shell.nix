{ pkgs ? import <nixpkgs> {} }:

with pkgs;

mkShell ({
  buildInputs = [
    ghc
    glibcLocales
    hlint
    stack
    zlib
  ];
}
// (if stdenv.isLinux
       then {
         LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
       } else {}
   )
)
