let
  elendel = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICZU5fDbqEkllipbknJy/Dm3Fcicb5gscVzmsFG/9RoA";
  kholinar = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOL8MzChayhcVTfZvE3/ExwXpq2+LbihjzUVlKeIGoOL";
  marabethia = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAUP1r937+PLiqdyUuqbYoyAs04/2AxuXS13grU+fvpA";
  thaylen-city = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIC/sIkgf7aYX/JcWWp/dCHgq7sJ5WDYYyWSn3DvkW4gB";
  nixosHosts = [
    elendel
    kholinar
    marabethia
  ];
  hosts = [
    elendel
    kholinar
    marabethia
    thaylen-city
  ];
  nixosPersonals = [
    kholinar
  ];
  personals = [
    kholinar
    thaylen-city
  ];
  servers = [
    elendel
    marabethia
  ];
  charlotte = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICDb17zAg3zwvdYHNZqXSGYKseCz5281Ha6oOYPbwFYD"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJLsSFEi4CGpkWIJxXJC78bhibrBRxClBbpS9n7PQGYL"
  ];
  users = charlotte;
in
{
  "secrets/passwords/users/charlotte.age".publicKeys = nixosHosts ++ users;
  "secrets/passwords/users/root.age".publicKeys = nixosHosts ++ users;

  "secrets/authorized_keys/charlotte.age".publicKeys = hosts ++ users;
  "secrets/authorized_keys/root.age".publicKeys = hosts ++ users;

  "secrets/passwords/networks.age".publicKeys = nixosPersonals ++ users;

  "secrets/files/programs/vpn/local.age".publicKeys = personals ++ users;
  "secrets/files/programs/vpn/global.age".publicKeys = personals ++ users;

  "secrets/passwords/services/accentor.age".publicKeys = [ elendel ] ++ users;

  "secrets/files/services/phone-push-url.age".publicKeys = hosts ++ users;

  "secrets/passwords/services/mail/charlotte_at_vanpetegem.be.age".publicKeys = [ marabethia ] ++ users;
  "secrets/passwords/services/mail/git_at_chvp.be.age".publicKeys = [ marabethia ] ++ users;
  "secrets/passwords/services/mail/hallo_at_estherdereys.be.age".publicKeys = [ marabethia ] ++ users;
  "secrets/passwords/services/mail/hallo_at_robbe.be.age".publicKeys = [ marabethia ] ++ users;
  "secrets/passwords/services/mail/huis_at_vanpetegem.me.age".publicKeys = [ marabethia ] ++ users;
  "secrets/passwords/services/mail/info_at_eenstweedrie.be.age".publicKeys = [ marabethia ] ++ users;
  "secrets/passwords/services/mail/noreply_at_vanpetegem.me.age".publicKeys = [ marabethia ] ++ users;
  "secrets/passwords/services/mail/peter_at_vanpetegem.me.age".publicKeys = [ marabethia ] ++ users;
  "secrets/passwords/services/mail/postbot_at_vanpetegem.be.age".publicKeys = [ marabethia ] ++ users;
  "secrets/passwords/services/mail/robbe_at_robbevanpetegem.be.age".publicKeys = [ marabethia ] ++ users;
  "secrets/passwords/services/mail/robbe_at_vanpetegem.be.age".publicKeys = [ marabethia ] ++ users;
  "secrets/passwords/services/mail/webmaster_at_vanpetegem.be.age".publicKeys = [ marabethia ] ++ users;
  "secrets/passwords/services/ssmtp-pass.age".publicKeys = nixosHosts ++ users;

  "secrets/passwords/services/acme.age".publicKeys = servers ++ users;

  "secrets/passwords/services/git/mail-password.age".publicKeys = [ marabethia ] ++ users;
  "secrets/passwords/services/git/token-file.age".publicKeys = [ marabethia ] ++ users;

  "secrets/passwords/services/murmur.age".publicKeys = [ marabethia ] ++ users;

  "secrets/passwords/services/nextcloud-admin.age".publicKeys = [ marabethia ] ++ users;

  "secrets/passwords/services/data-basic-auth.age".publicKeys = [ elendel ] ++ users;

  "secrets/files/programs/ssh/host_configuration.age".publicKeys = nixosHosts ++ users;

  "secrets/files/programs/transmission/config.json.age".publicKeys = [ elendel ] ++ users;

  "secrets/files/services/matrix-appservice-slack/config.yml.age".publicKeys = [ marabethia ] ++ users;
  "secrets/files/services/matrix-appservice-slack/registration.yml.age".publicKeys = [ marabethia ] ++ users;
  "secrets/files/services/matrix-hookshot/config.yml.age".publicKeys = [ marabethia ] ++ users;
  "secrets/files/services/matrix-hookshot/passkey.pem.age".publicKeys = [ marabethia ] ++ users;
  "secrets/files/services/matrix-hookshot/registration.yml.age".publicKeys = [ marabethia ] ++ users;
  "secrets/files/services/matrix-synapse/config.yml.age".publicKeys = [ marabethia ] ++ users;
  "secrets/files/services/mautrix-whatsapp/config.yml.age".publicKeys = [ marabethia ] ++ users;
  "secrets/files/services/mautrix-whatsapp/registration.yml.age".publicKeys = [ marabethia ] ++ users;

  "secrets/files/wireguard/elendel.privkey.age".publicKeys = [ elendel ] ++ users;
  "secrets/files/wireguard/kholinar.privkey.age".publicKeys = [ kholinar ] ++ users;
  "secrets/files/wireguard/marabethia.privkey.age".publicKeys = [ marabethia ] ++ users;
  "secrets/files/wireguard/thaylen-city.privkey.age".publicKeys = [ thaylen-city ] ++ users;
  "secrets/files/wireguard/psk.age".publicKeys = hosts ++ users;

  "secrets/data-access/ssh_host_rsa_key.age".publicKeys = [ elendel ] ++ users;
  "secrets/data-access/ssh_host_rsa_key.pub.age".publicKeys = [ elendel ] ++ users;
  "secrets/data-access/ssh_host_ed25519_key.age".publicKeys = [ elendel ] ++ users;
  "secrets/data-access/ssh_host_ed25519_key.pub.age".publicKeys = [ elendel ] ++ users;
  "secrets/data-access/authorized_keys.age".publicKeys = [ elendel ] ++ users;
  "secrets/data-access/password_file.age".publicKeys = [ elendel ] ++ users;
  "secrets/data-access/readonly_authorized_keys.age".publicKeys = [ elendel ] ++ users;
  "secrets/data-access/readonly_password_file.age".publicKeys = [ elendel ] ++ users;
  "secrets/data-access/create_torrent.age".publicKeys = [ elendel ] ++ users;
}
