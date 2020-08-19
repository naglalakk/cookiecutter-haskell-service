{ pkgs ? import ./packages.nix {}, mkDerivation
, aeson, base, base64-bytestring, bcrypt, bloodhound
, bytestring, containers, dotenv, fast-logger, hedis, hpack
, hslogger, http-client, http-types, monad-control, monad-logger
, mtl, optparse-applicative, persistent, persistent-postgresql
, persistent-template, safe, servant, servant-auth, servant-server
, stdenv, text, time, transformers, unordered-containers
, utf8-string, wai, wai-cors, wai-extra, warp
}:
let 
  gitignoreSrc = pkgs.fetchFromGitHub { 
    owner = "hercules-ci";
    repo = "gitignore.nix";
    rev = "7415c4feb127845553943a3856cbc5cb967ee5e0";
    sha256 = "sha256:1zd1ylgkndbb5szji32ivfhwh04mr1sbgrnvbrqpmfb67g2g3r9i";
  };
  inherit (import gitignoreSrc { inherit (pkgs) lib; }) gitignoreSource;
in
  mkDerivation {
    pname = "{{ cookiecutter.package_name }}";
    version = "{{ cookiecutter.version }}";
    src = gitignoreSource ./.;
    isLibrary = true;
    isExecutable = true;
    doHaddock = false;
    libraryHaskellDepends = [
      aeson base base64-bytestring bcrypt bloodhound bytestring
      containers dotenv fast-logger hedis hslogger http-client http-types
      monad-control monad-logger mtl optparse-applicative persistent
      persistent-postgresql persistent-template safe servant servant-auth
      servant-server text time transformers unordered-containers
      utf8-string wai wai-cors wai-extra warp
    ];
    libraryToolDepends = [ hpack ];
    executableHaskellDepends = [
      base base64-bytestring bcrypt bytestring dotenv fast-logger hedis
      hslogger http-types monad-logger mtl optparse-applicative
      persistent persistent-postgresql persistent-template safe text time
      utf8-string wai wai-cors wai-extra warp
    ];
    prePatch = "hpack";
    description = "{{ cookiecutter.description }}";
    license = stdenv.lib.licenses.mit;
  }
