{
  # Mostly copied from http://zef.me/blog/5981/deploying-a-simple-node-js-application-with-nixops
  # Name of our deployment
  network.description = "modernator-haskell";
  # Enable rolling back to previous versions of our infrastructure
  network.enableRollback = true;

  # It consists of a single server named 'modernator-haskell'
  modernator-haskell =
    # Every server gets passed a few arguments, including a reference
    # to nixpkgs (pkgs)
    { config, pkgs, ... }:
    let
      # We import our custom packages from ./default passing pkgs as argument
      app = import ./default.nix { nixpkgs = pkgs; };
      port = "8080";
    in
    {
      environment.systemPackages = with pkgs; [ screen htop app ];

      # Only open up port 22 for ssh, this isn't a webserver...
      networking.firewall.enable = true;
      networking.firewall.allowedTCPPorts = [ 22 80 8080 ];
      networking.firewall.allowPing = true;
      networking.firewall.extraCommands = ''
        iptables -t nat -A PREROUTING -p tcp --dport 80 -j REDIRECT --to-port ${port}
      '';

      systemd.services.modernator-haskell = {
        description = "A webservice for moderating Q&A sessions";

        # Start the service after the network is available
        after = [ "network.target" ];

        environment = { MODERNATOR_PORT = port; MODERNATOR_STATE_DIR = "/home/modernator/state"; };

        serviceConfig = {
          # The actual command to run
          ExecStart = "${app}/bin/modernator-haskell";
          # For security reasons we'll run this process as a special 'modernator' user
          User = "modernator";
          Group = "modernator";
          Restart = "always";
        };
      };

      # And lastly we ensure the user we run our application as is created
      users.extraUsers = {
        modernator = {
          home = "/home/modernator";
          createHome = true;
          shell = "/bin/sh";
          password = "password";
        };
      };
    };
}

