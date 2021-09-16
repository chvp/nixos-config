let
  kharbranth = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKWu55zjDTl0qr5+kWNzuxGe5qem40ML8ELohapW/xug";
  kholinar = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOL8MzChayhcVTfZvE3/ExwXpq2+LbihjzUVlKeIGoOL";
  lasting-integrity = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMKJmeY7j5LxWVv3fKzqG4Bvg/ZhOp8iwk0utpyMWMSk";
  urithiru = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOrzOpyzDc5BVtAeb5//PnMRcp+9B+DjfU7p2YpaH6a2";
  hosts = [
    kharbranth
    kholinar
    lasting-integrity
    urithiru
  ];
  laptops = [
    kharbranth
    kholinar
  ];
  servers = [
    lasting-integrity
    urithiru
  ];

  charlotte = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICDb17zAg3zwvdYHNZqXSGYKseCz5281Ha6oOYPbwFYD"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJY5nXR/V6wcMRxugD7GTOF8kwfGnAT2CRuJ2Qi60vsm"
  ];
  users = charlotte;
in
{
  "secrets/passwords/users/charlotte.age".publicKeys = hosts ++ users;
  "secrets/passwords/users/root.age".publicKeys = hosts ++ users;

  "secrets/authorized_keys/charlotte.age".publicKeys = hosts ++ users;
  "secrets/authorized_keys/root.age".publicKeys = hosts ++ users;

  "secrets/passwords/ugent-mount-credentials.age".publicKeys = laptops ++ users;
  "secrets/passwords/ugent-vpn.age".publicKeys = laptops ++ users;
  "secrets/files/programs/vpn/local.age".publicKeys = laptops ++ users;
  "secrets/files/programs/vpn/global.age".publicKeys = laptops ++ users;

  "secrets/passwords/services/accentor.age".publicKeys = [ urithiru ] ++ users;

  "secrets/passwords/services/ssmtp-pass.age".publicKeys = hosts ++ users;

  "secrets/passwords/services/acme.age".publicKeys = servers ++ users;

  "secrets/passwords/services/nextcloud-admin.age".publicKeys = [ lasting-integrity ] ++ users;

  "secrets/passwords/services/syncthing-basic-auth.age".publicKeys = [ lasting-integrity ] ++ users;

  "secrets/passwords/services/data-basic-auth.age".publicKeys = [ urithiru ] ++ users;

  "secrets/files/programs/ssh/host_configuration.age".publicKeys = hosts ++ users;

  "secrets/files/services/matrix-appservice-slack/config.yml.age".publicKeys = [ lasting-integrity ] ++ users;
  "secrets/files/services/matrix-appservice-slack/registration.yml.age".publicKeys = [ lasting-integrity ] ++ users;
  "secrets/files/services/matrix-synapse/config.yml.age".publicKeys = [ lasting-integrity ] ++ users;
  "secrets/files/services/mautrix-whatsapp/config.yml.age".publicKeys = [ lasting-integrity ] ++ users;
  "secrets/files/services/mautrix-whatsapp/registration.yml.age".publicKeys = [ lasting-integrity ] ++ users;

  "secrets/data-access/ssh_host_rsa_key.age".publicKeys = [ urithiru ] ++ users;
  "secrets/data-access/ssh_host_rsa_key.pub.age".publicKeys = [ urithiru ] ++ users;
  "secrets/data-access/ssh_host_ed25519_key.age".publicKeys = [ urithiru ] ++ users;
  "secrets/data-access/ssh_host_ed25519_key.pub.age".publicKeys = [ urithiru ] ++ users;
  "secrets/data-access/authorized_keys.age".publicKeys = [ urithiru ] ++ users;
  "secrets/data-access/password_file.age".publicKeys = [ urithiru ] ++ users;
  "secrets/data-access/create_torrent.age".publicKeys = [ urithiru ] ++ users;
}
