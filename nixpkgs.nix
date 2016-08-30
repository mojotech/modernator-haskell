let source = ''
      {
        "owner": "NixOS",
        "repo": "nixpkgs-channels",
        "rev": "4574f22841d23a12fc135af74f198c4313002f4a",
        "sha256": "0xmvwg4j2yc18658y1afdl1yrgkpmh7zwx590mgzbgvr7c6claqk"
      }
      '';
in
import ((import <nixpkgs> {}).fetchFromGitHub (builtins.fromJSON (source)))
