# NixOS config

## Secrets

There are two types of secrets in this repository. Secret secrets, and
secret configuration.

Secret secrets should never be world-readable, even to users who are
logged in to one of the hosts managed by this configuration. These are
generally managed by agenix, allowing them to still be put in the nix
store.

Secret configuration is generally more security through obscurity
(e.g. some services that I run that I don't want the whole world to
know what ports they run on). These are managed with git-crypt and are
files that end in `secret.nix`.

## Setting up a new dev environment

* Create a new `*.nix` file in the shells directory that describes the environment (this is the hard part).

* Execute `use_nix > .envrc` to initialize the `.envrc` file.

* Execute `ln -s /path/to/correct/file.nix shell.nix`.

* Execute `direnv allow` to load the `.envrc` file which in turn loads your environment.

## Setting up ZFS

1. Create three partitions:
   * Boot
   * Swap
   * ZFS

   For example:
   ```shell
   sgdisk -n 0:0:+512MiB -t 0:EF00 -c 0:boot $DISK
   sgdisk -n 0:0:+32GiB -t 0:8200 -c 0:swap $DISK
   sgdisk -n 0:0:0 -t 0:BF01 -c 0:ZFS $DISK
   ```

2. Configure swap and boot as usual.

3. Create ZPool:
   ```shell
   zpool create -O mountpoint=none -O encryption=aes-256-gcm -O keyformat=passphrase rpool $ZFS_PART
   ```
   Leave out `-O encryption=aes-256-gcm -O keyformat=passphrase` if you don't want to fully encrypt the ZFS partition.

4. Create datasets:
   ```shell
   zfs create -o mountpoint=legacy rpool/local/root
   zfs snapshot rpool/local/root@blank
   zfs create -o mountpoint=legacy rpool/local/nix
   zfs set compression=lz4 rpool/local/nix
   zfs create -o mountpoint=legacy rpool/local/cache
   zfs set compression=lz4 rpool/local/cache
   zfs create -o mountpoint=legacy rpool/safe/data
   zfs set compression=lz4 rpool/local/data
   ```
5. Mount datasets:
   ```shell
   mount -t zfs rpool/local/root /mnt
   mkdir /mnt/nix
   mount -t zfs rpool/local/nix /mnt/nix
   mkdir /mnt/boot
   mount $BOOT_PART /mnt/boot
   mkdir /mnt/cache
   mount -t zfs rpool/local/cache /mnt/cache
   mkdir /mnt/data
   mount -t zfs rpool/safe/data /mnt/data
   ```
6. Configure Host ID

   Set `networking.hostid` in the nixos config to `head -c 8 /etc/machine-id`.
