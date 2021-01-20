self: super:
let
  inherit (self) fetchFromGitHub;

  bytestring-lexing-repo = fetchFromGitHub {
    owner = "juspay";
    repo = "bytestring-lexing";
    rev = "0a46db1139011736687cb50bbd3877d223bcb737";
    sha256 = "1jrwhlp8xs4m21xfr843278j3i7h4sxyjpq67l6lzc36pqan9zlz";
  };

  bytestring-lexing-path = bytestring-lexing-repo;

  mysql-haskell-repo = fetchFromGitHub {
    owner = "juspay";
    repo = "mysql-haskell";
    rev = "dce6811d8d5a492bb57ea4b7453734a9bcc84dd9";
  };

  mysql-haskell-path = mysql-haskell-repo;

  beam-mysql-path = ../.;
in
super.eulerBuild.mkEulerHaskellOverlay self super
  (hself: hsuper:
    let 
    in {
      # Needed for floating point fix in mysql-haskell
      bytestring-lexing = self.eulerBuild.fastBuildExternal {
        drv = hself.callCabal2nix "bytestring-lexing" bytestring-lexing-path { };
      };

      # Uses bytestring-lexing
      binary-parsers = self.eulerBuild.fastBuildExternal {
        drv = hsuper.binary-parsers;
      };
      # Uses binary-parsers
      wire-streams = self.eulerBuild.fastBuildExternal {
        drv = hsuper.wire-streams;
      };

      mysql-haskell = self.eulerBuild.fastBuildExternal {
        drv = hself.callCabal2nix "mysql-haskell" mysql-haskell-path { };
      };

      mason =
        self.eulerBuild.fastBuildExternal {
          drv = hself.callHackageDirect {
            pkg = "mason";
            ver = "0.2.3";
            sha256 = "1dcd3n1lxlpjsz92lmr1nsx29mwwglim0gask04668sdiarr3x1v";
          } { };
        };

      record-dot-preprocessor =
        self.eulerBuild.fastBuildExternal {
          drv = hself.callHackageDirect {
            pkg = "record-dot-preprocessor";
            ver = "0.2.7";
            sha256 = "0dyn5wpn0p4sc1yw4zq9awrl2aa3gd3jamllfxrg31v3i3l6jvbw";
          } { };
        };

      beam-mysql = self.eulerBuild.fastBuildExternal {
        drv = hself.callCabal2nix "beam-mysql" beam-mysql-path { };
      };
  })
