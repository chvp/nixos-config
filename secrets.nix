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

  "secrets/passwords/networks.age".publicKeys = laptops ++ users;

  "secrets/passwords/ugent-mount-credentials.age".publicKeys = laptops ++ users;
  "secrets/passwords/ugent-vpn.age".publicKeys = laptops ++ users;
  "secrets/files/programs/vpn/local.age".publicKeys = laptops ++ users;
  "secrets/files/programs/vpn/global.age".publicKeys = laptops ++ users;

  "secrets/passwords/services/accentor.age".publicKeys = [ urithiru ] ++ users;

  "secrets/passwords/services/mail/charlotte_at_vanpetegem.me.age".publicKeys = [ lasting-integrity ] ++ users;
  "secrets/passwords/services/mail/expenses-noreply_at_vanpetegem.me.age".publicKeys = [ lasting-integrity ] ++ users;
  "secrets/passwords/services/mail/huis_at_vanpetegem.me.age".publicKeys = [ lasting-integrity ] ++ users;
  "secrets/passwords/services/mail/noreply_at_vanpetegem.me.age".publicKeys = [ lasting-integrity ] ++ users;
  "secrets/passwords/services/mail/peter_at_vanpetegem.me.age".publicKeys = [ lasting-integrity ] ++ users;
  "secrets/passwords/services/mail/postbot_at_vanpetegem.me.age".publicKeys = [ lasting-integrity ] ++ users;
  "secrets/passwords/services/mail/robbe_at_vanpetegem.me.age".publicKeys = [ lasting-integrity ] ++ users;
  "secrets/passwords/services/mail/ugent_at_cvpetegem.be.age".publicKeys = [ lasting-integrity ] ++ users;
  "secrets/passwords/services/mail/webmaster_at_vanpetegem.me.age".publicKeys = [ lasting-integrity ] ++ users;
  "secrets/passwords/services/ssmtp-pass.age".publicKeys = hosts ++ users;

  "secrets/passwords/services/acme.age".publicKeys = servers ++ users;

  "secrets/passwords/services/git/initial-root-password.age".publicKeys = [ lasting-integrity ] ++ users;
  "secrets/passwords/services/git/db.age".publicKeys = [ lasting-integrity ] ++ users;
  "secrets/passwords/services/git/jws.age".publicKeys = [ lasting-integrity ] ++ users;
  "secrets/passwords/services/git/otp.age".publicKeys = [ lasting-integrity ] ++ users;
  "secrets/passwords/services/git/secret.age".publicKeys = [ lasting-integrity ] ++ users;

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

  "secrets/passwords/services/syncthing-basic-auth.age".publicKeys = [ lasting-integrity ] ++ users;

  "secrets/files/services/tunnel/key.age".publicKeys = [ lasting-integrity ] ++ users;
  "secrets/files/services/tunnel/env.age".publicKeys = [ lasting-integrity ] ++ users;

  "secrets/passwords/services/data-basic-auth.age".publicKeys = [ urithiru ] ++ users;

  "secrets/files/programs/ssh/host_configuration.age".publicKeys = hosts ++ users;

  "secrets/files/services/matrix-appservice-slack/config.yml.age".publicKeys = [ lasting-integrity ] ++ users;
  "secrets/files/services/matrix-appservice-slack/registration.yml.age".publicKeys = [ lasting-integrity ] ++ users;
  "secrets/files/services/matrix-synapse/config.yml.age".publicKeys = [ lasting-integrity ] ++ users;
  "secrets/files/services/mautrix-whatsapp/config.yml.age".publicKeys = [ lasting-integrity ] ++ users;
  "secrets/files/services/mautrix-whatsapp/registration.yml.age".publicKeys = [ lasting-integrity ] ++ users;

  "secrets/files/wireguard/kharbranth.privkey.age".publicKeys = [ kharbranth ] ++ users;
  "secrets/files/wireguard/kholinar.privkey.age".publicKeys = [ kholinar ] ++ users;
  "secrets/files/wireguard/lasting-integrity.privkey.age".publicKeys = [ lasting-integrity ] ++ users;
  "secrets/files/wireguard/urithiru.privkey.age".publicKeys = [ urithiru ] ++ users;
  "secrets/files/wireguard/psk.age".publicKeys = hosts ++ users;
  "secrets/files/wireguard/udp2raw.age".publicKeys = hosts ++ users;

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
