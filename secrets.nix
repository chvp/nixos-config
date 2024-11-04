let
  kholinar = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOL8MzChayhcVTfZvE3/ExwXpq2+LbihjzUVlKeIGoOL";
  lasting-integrity = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMKJmeY7j5LxWVv3fKzqG4Bvg/ZhOp8iwk0utpyMWMSk";
  thaylen-city = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIC/sIkgf7aYX/JcWWp/dCHgq7sJ5WDYYyWSn3DvkW4gB";
  urithiru = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOrzOpyzDc5BVtAeb5//PnMRcp+9B+DjfU7p2YpaH6a2";
  nixosHosts = [
    kholinar
    lasting-integrity
    urithiru
  ];
  hosts = [
    kholinar
    lasting-integrity
    thaylen-city
    urithiru
  ];
  nixosPersonals = [
    kholinar
  ];
  personals = [
    kholinar
    thaylen-city
  ];
  servers = [
    lasting-integrity
    urithiru
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

  "secrets/passwords/services/accentor.age".publicKeys = [ urithiru ] ++ users;

  "secrets/files/services/phone-push-url.age".publicKeys = hosts ++ users;

  "secrets/passwords/services/mail/charlotte_at_vanpetegem.be.age".publicKeys = [ lasting-integrity ] ++ users;
  "secrets/passwords/services/mail/hallo_at_estherdereys.be.age".publicKeys = [ lasting-integrity ] ++ users;
  "secrets/passwords/services/mail/hallo_at_robbe.be.age".publicKeys = [ lasting-integrity ] ++ users;
  "secrets/passwords/services/mail/huis_at_vanpetegem.me.age".publicKeys = [ lasting-integrity ] ++ users;
  "secrets/passwords/services/mail/info_at_eenstweedrie.be.age".publicKeys = [ lasting-integrity ] ++ users;
  "secrets/passwords/services/mail/noreply_at_vanpetegem.me.age".publicKeys = [ lasting-integrity ] ++ users;
  "secrets/passwords/services/mail/peter_at_vanpetegem.me.age".publicKeys = [ lasting-integrity ] ++ users;
  "secrets/passwords/services/mail/postbot_at_vanpetegem.be.age".publicKeys = [ lasting-integrity ] ++ users;
  "secrets/passwords/services/mail/robbe_at_robbevanpetegem.be.age".publicKeys = [ lasting-integrity ] ++ users;
  "secrets/passwords/services/mail/robbe_at_vanpetegem.me.age".publicKeys = [ lasting-integrity ] ++ users;
  "secrets/passwords/services/mail/webmaster_at_vanpetegem.be.age".publicKeys = [ lasting-integrity ] ++ users;
  "secrets/passwords/services/ssmtp-pass.age".publicKeys = nixosHosts ++ users;

  "secrets/passwords/services/acme.age".publicKeys = servers ++ users;

  "secrets/passwords/services/git/initial-root-password.age".publicKeys = [ lasting-integrity ] ++ users;
  "secrets/passwords/services/git/db.age".publicKeys = [ lasting-integrity ] ++ users;
  "secrets/passwords/services/git/jws.age".publicKeys = [ lasting-integrity ] ++ users;
  "secrets/passwords/services/git/otp.age".publicKeys = [ lasting-integrity ] ++ users;
  "secrets/passwords/services/git/secret.age".publicKeys = [ lasting-integrity ] ++ users;
  "secrets/passwords/services/gitlab-runner/registration.age".publicKeys = [ urithiru ] ++ users;

  "secrets/passwords/services/mastodon/otp.age".publicKeys = [ lasting-integrity ] ++ users;
  "secrets/passwords/services/mastodon/key.age".publicKeys = [ lasting-integrity ] ++ users;
  "secrets/passwords/services/mastodon/vapid-public.age".publicKeys = [ lasting-integrity ] ++ users;
  "secrets/passwords/services/mastodon/vapid-private.age".publicKeys = [ lasting-integrity ] ++ users;

  "secrets/passwords/services/garmin2influx-env.age".publicKeys = [ lasting-integrity ] ++ users;
  "secrets/passwords/services/grafana/smtp.age".publicKeys = [ lasting-integrity ] ++ users;
  "secrets/passwords/services/grafana/admin-password.age".publicKeys = [ lasting-integrity ] ++ users;
  "secrets/passwords/services/grafana/secret-key.age".publicKeys = [ lasting-integrity ] ++ users;
  "secrets/passwords/services/telegraf-env.age".publicKeys = hosts ++ users;

  "secrets/passwords/services/nextcloud-admin.age".publicKeys = [ lasting-integrity ] ++ users;

  "secrets/passwords/services/data-basic-auth.age".publicKeys = [ urithiru ] ++ users;

  "secrets/files/programs/ssh/host_configuration.age".publicKeys = nixosHosts ++ users;

  "secrets/files/programs/transmission/config.json.age".publicKeys = [ urithiru ] ++ users;

  "secrets/files/services/matrix-appservice-slack/config.yml.age".publicKeys = [ lasting-integrity ] ++ users;
  "secrets/files/services/matrix-appservice-slack/registration.yml.age".publicKeys = [ lasting-integrity ] ++ users;
  "secrets/files/services/matrix-hookshot/config.yml.age".publicKeys = [ lasting-integrity ] ++ users;
  "secrets/files/services/matrix-hookshot/passkey.pem.age".publicKeys = [ lasting-integrity ] ++ users;
  "secrets/files/services/matrix-hookshot/registration.yml.age".publicKeys = [ lasting-integrity ] ++ users;
  "secrets/files/services/matrix-synapse/config.yml.age".publicKeys = [ lasting-integrity ] ++ users;
  "secrets/files/services/mautrix-whatsapp/config.yml.age".publicKeys = [ lasting-integrity ] ++ users;
  "secrets/files/services/mautrix-whatsapp/registration.yml.age".publicKeys = [ lasting-integrity ] ++ users;

  "secrets/files/wireguard/kholinar.privkey.age".publicKeys = [ kholinar ] ++ users;
  "secrets/files/wireguard/lasting-integrity.privkey.age".publicKeys = [ lasting-integrity ] ++ users;
  "secrets/files/wireguard/thaylen-city.privkey.age".publicKeys = [ thaylen-city ] ++ users;
  "secrets/files/wireguard/urithiru.privkey.age".publicKeys = [ urithiru ] ++ users;
  "secrets/files/wireguard/psk.age".publicKeys = hosts ++ users;

  "secrets/data-access/ssh_host_rsa_key.age".publicKeys = [ urithiru ] ++ users;
  "secrets/data-access/ssh_host_rsa_key.pub.age".publicKeys = [ urithiru ] ++ users;
  "secrets/data-access/ssh_host_ed25519_key.age".publicKeys = [ urithiru ] ++ users;
  "secrets/data-access/ssh_host_ed25519_key.pub.age".publicKeys = [ urithiru ] ++ users;
  "secrets/data-access/authorized_keys.age".publicKeys = [ urithiru ] ++ users;
  "secrets/data-access/password_file.age".publicKeys = [ urithiru ] ++ users;
  "secrets/data-access/readonly_authorized_keys.age".publicKeys = [ urithiru ] ++ users;
  "secrets/data-access/readonly_password_file.age".publicKeys = [ urithiru ] ++ users;
  "secrets/data-access/create_torrent.age".publicKeys = [ urithiru ] ++ users;
}
