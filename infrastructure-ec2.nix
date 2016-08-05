let
  # Insert your AWS access key here
  accessKey = "personal";
in {
  modernator-haskell = { resources, ... }:
    { deployment.targetEnv = "ec2";
      deployment.ec2.region = "us-east-1";
      deployment.ec2.instanceType = "t2.nano";
      deployment.ec2.accessKeyId = accessKey;
      # We'll let NixOps generate a keypair automatically
      deployment.ec2.keyPair = resources.ec2KeyPairs.modernator-haskell-kp.name;
      deployment.ec2.securityGroups = [ "ssh" "HTTP/HTTPS" ];
    };

  # Here we create a keypair in the same region as our deployment
  resources.ec2KeyPairs.modernator-haskell-kp = {
    region = "us-east-1";
    accessKeyId = accessKey;
  };
}
