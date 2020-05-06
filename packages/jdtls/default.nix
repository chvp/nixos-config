{ stdenv, pkgs, ... }:

stdenv.mkDerivation rec {
  pname = "jdtls";
  version = "0.55.0";
  src = pkgs.fetchurl {
    url = "http://download.eclipse.org/${pname}/milestones/${version}/jdt-language-server-${version}-202004300028.tar.gz";
    sha256 = "0kxx4l7yqvzxbcb0j8yvrpvq6rba50j173dl71p4kqz14878m17w";
  };
  sourceRoot = ".";
  buildPhase = "true";
  installPhase = ''
    mkdir -p $out/share/java/jdtls
    mv config_* features plugins $out/share/java/jdtls
    mkdir $out/bin
    cat >$out/bin/jdtls <<HERE
    #!${pkgs.bash}/bin/bash

    tmp_dir=\$(mktemp -d /tmp/jdtls.XXXXX)
    cp -R $out/share/java/jdtls/config_linux \$tmp_dir
    chmod -R u=rwX \$tmp_dir
    trap "{ rm -rf \$tmp_dir; }" 0

    ${pkgs.jdk11}/bin/java \
      -Declipse.application=org.eclipse.jdt.ls.core.id1 \
      -Dosgi.bundles.defaultStartLevel=4 \
      -Declipse.product=org.eclipse.jdt.ls.core.product \
      -noverify \
      -Xms1G \
      -jar $out/share/java/jdtls/plugins/org.eclipse.equinox.launcher_*.jar \
      -configuration "\$tmp_dir/config_linux" \
      "\$@"
    HERE
    chmod +x $out/bin/jdtls
  '';
}
