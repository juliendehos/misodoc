 
let

  miso-src = fetchTarball {
    url = https://github.com/dmjio/miso/archive/530445a2a6bca109e25a0fa25bb5cf97e498ef84.tar.gz;
    sha256 = "sha256:0ipw2w9s4h4dgay1hnx0bjswc5k0lazl03zdwfzxqrjpgvd48wby";
    # should match cabal.project
  };
  
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = self: super: rec {
          miso = self.callCabal2nixWithOptions "miso" miso-src "-ftemplate-haskell" {};
        };
      };
    };
  };

  channel = <nixpkgs>;
  # channel = fetchTarball "https://github.com/NixOS/nixpkgs/archive/25.05.tar.gz";

  pkgs = import channel { inherit config; };

in pkgs

