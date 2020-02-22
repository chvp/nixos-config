{ stdenv, pkgs, ... }:

stdenv.mkDerivation rec {
  pname = "jdtls";
  version = "0.50.0";
  src = pkgs.fetchurl {
    url = "http://download.eclipse.org/${pname}/milestones/${version}/jdt-language-server-${version}-202002171858.tar.gz";
    sha256 = "bf72084dc7988f76e01668d778c826c7490c287ee96a9f82e9e11afb6b722ab7";
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
