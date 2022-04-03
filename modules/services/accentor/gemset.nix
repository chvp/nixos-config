{
  actioncable = {
    dependencies = [ "actionpack" "activesupport" "nio4r" "websocket-driver" ];
    groups = [ "default" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "05abj034rvyc1hgywp1gvj8bkxgp151aryxgmivxzk6sd29saq9q";
      type = "gem";
    };
    version = "7.0.2.3";
  };
  actionmailbox = {
    dependencies = [ "actionpack" "activejob" "activerecord" "activestorage" "activesupport" "mail" "net-imap" "net-pop" "net-smtp" ];
    groups = [ "default" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "0ryx9y27m6bp6mwkk0v6sq48kpvq9i15pwhfvq2gvkph1d41vx1k";
      type = "gem";
    };
    version = "7.0.2.3";
  };
  actionmailer = {
    dependencies = [ "actionpack" "actionview" "activejob" "activesupport" "mail" "net-imap" "net-pop" "net-smtp" "rails-dom-testing" ];
    groups = [ "default" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "0qsns7caqymk0zm5nn5dylqzgjvdjs96fp8l0ycadi5j2q93klxg";
      type = "gem";
    };
    version = "7.0.2.3";
  };
  actionpack = {
    dependencies = [ "actionview" "activesupport" "rack" "rack-test" "rails-dom-testing" "rails-html-sanitizer" ];
    groups = [ "default" "development" "test" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "1yb36akfg61zq210sc9y1l3a0jz10m61a81iqxiczknsfh9d76rb";
      type = "gem";
    };
    version = "7.0.2.3";
  };
  actiontext = {
    dependencies = [ "actionpack" "activerecord" "activestorage" "activesupport" "globalid" "nokogiri" ];
    groups = [ "default" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "0bky0ixc4b1si0zhywm8cds73bvxprga86yap3z8p95irarziybq";
      type = "gem";
    };
    version = "7.0.2.3";
  };
  actionview = {
    dependencies = [ "activesupport" "builder" "erubi" "rails-dom-testing" "rails-html-sanitizer" ];
    groups = [ "default" "development" "test" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "1nldiqlj9jlnfvqblhk6c4md77nqri24ln10ajhfbgrx9b6h140h";
      type = "gem";
    };
    version = "7.0.2.3";
  };
  active_model_serializers = {
    dependencies = [ "actionpack" "activemodel" "case_transform" "jsonapi-renderer" ];
    groups = [ "default" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "0xdp7cpj3yj3wl4vj0nqq44kzjavlxi1wq3cf9zp0whkir0ym0gy";
      type = "gem";
    };
    version = "0.10.13";
  };
  activejob = {
    dependencies = [ "activesupport" "globalid" ];
    groups = [ "default" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "12ycizvhxcp7mz99g6ap9y67z5n2wy0bbbm8s5il765xsfizi76k";
      type = "gem";
    };
    version = "7.0.2.3";
  };
  activemodel = {
    dependencies = [ "activesupport" ];
    groups = [ "default" "development" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "1r2657xaa6zfvp3y04nrl4hnjpvd9m9sl7hs83q4nbnclkgzhb4w";
      type = "gem";
    };
    version = "7.0.2.3";
  };
  activerecord = {
    dependencies = [ "activemodel" "activesupport" ];
    groups = [ "default" "development" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "104jdjghliw33zmivlmvq70l40dyyz6gh1qsb0z82n9wyp861s18";
      type = "gem";
    };
    version = "7.0.2.3";
  };
  activestorage = {
    dependencies = [ "actionpack" "activejob" "activerecord" "activesupport" "marcel" "mini_mime" ];
    groups = [ "default" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "143bfqq9m1mvmvm4s7qvgqzfl8fqa46a45kz5f4v0zfbflqx6f6v";
      type = "gem";
    };
    version = "7.0.2.3";
  };
  activesupport = {
    dependencies = [ "concurrent-ruby" "i18n" "minitest" "tzinfo" ];
    groups = [ "default" "development" "test" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "1jpydd414j0fig3r0f6ci67mchclg6cq2qgqbq9zplrbg40pzfi8";
      type = "gem";
    };
    version = "7.0.2.3";
  };
  annotate = {
    dependencies = [ "activerecord" "rake" ];
    groups = [ "development" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "1lw0fxb5mirsdp3bp20gjyvs7clvi19jbxnrm2ihm20kzfhvlqcs";
      type = "gem";
    };
    version = "3.2.0";
  };
  ast = {
    groups = [ "default" "development" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "04nc8x27hlzlrr5c2gn7mar4vdr0apw5xg22wp6m8dx3wqr04a0y";
      type = "gem";
    };
    version = "2.4.2";
  };
  bcrypt = {
    groups = [ "default" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "1rakdhrnlclrpy7sihi9ipwdh7fjkkvzga171464lq6rzp07cf65";
      type = "gem";
    };
    version = "3.1.17";
  };
  bootsnap = {
    dependencies = [ "msgpack" ];
    groups = [ "default" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "0bjhh8pngmvnrsri2h6a753pgv0xdkbbgi1bmv6c7q137sp37jbg";
      type = "gem";
    };
    version = "1.11.1";
  };
  builder = {
    groups = [ "default" "development" "test" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "045wzckxpwcqzrjr353cxnyaxgf0qg22jh00dcx7z38cys5g1jlr";
      type = "gem";
    };
    version = "3.2.4";
  };
  byebug = {
    groups = [ "development" "test" ];
    platforms = [{
      engine = "maglev";
    }
      {
        engine = "mingw";
      }
      {
        engine = "mingw";
      }
      {
        engine = "ruby";
      }];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "0nx3yjf4xzdgb8jkmk2344081gqr22pgjqnmjg2q64mj5d6r9194";
      type = "gem";
    };
    version = "11.1.3";
  };
  case_transform = {
    dependencies = [ "activesupport" ];
    groups = [ "default" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "0fzyws6spn5arqf6q604dh9mrj84a36k5hsc8z7jgcpfvhc49bg2";
      type = "gem";
    };
    version = "0.2";
  };
  codecov = {
    dependencies = [ "simplecov" ];
    groups = [ "test" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "1pi2dmqxjw5wsn65yx5qz5ks5msqflj0zxvk11r3cxwgacvj3hys";
      type = "gem";
    };
    version = "0.6.0";
  };
  concurrent-ruby = {
    groups = [ "default" "development" "test" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "0nwad3211p7yv9sda31jmbyw6sdafzmdi2i2niaz6f0wk5nq9h0f";
      type = "gem";
    };
    version = "1.1.9";
  };
  crass = {
    groups = [ "default" "development" "test" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "0pfl5c0pyqaparxaqxi6s4gfl21bdldwiawrc0aknyvflli60lfw";
      type = "gem";
    };
    version = "1.0.6";
  };
  delayed_cron_job = {
    dependencies = [ "fugit" ];
    groups = [ "default" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "0vq865hbld0qinzflzknbxzh3mji2swzfvdwlmgr96pdw990qrgj";
      type = "gem";
    };
    version = "0.9.0";
  };
  delayed_job = {
    dependencies = [ "activesupport" ];
    groups = [ "default" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "1f1vzxi1xcm1mq7nw3xr9j4q6a5pr5xply15s0n1hl1gahsgnlfa";
      type = "gem";
    };
    version = "4.1.10";
  };
  delayed_job_active_record = {
    dependencies = [ "activerecord" "delayed_job" ];
    groups = [ "default" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "0wh1146hg0b85zv336dn00jx9mzw5ma0maj67is7bvz5l35hd6yk";
      type = "gem";
    };
    version = "4.1.7";
  };
  digest = {
    groups = [ "default" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "00vwzvxgby22h7jhwadqqf9ssbkp3ag2pl4g7q3zf1y8mlk7rk39";
      type = "gem";
    };
    version = "3.1.0";
  };
  docile = {
    groups = [ "default" "test" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "1lxqxgq71rqwj1lpl9q1mbhhhhhhdkkj7my341f2889pwayk85sz";
      type = "gem";
    };
    version = "1.4.0";
  };
  erubi = {
    groups = [ "default" "development" "test" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "09l8lz3j00m898li0yfsnb6ihc63rdvhw3k5xczna5zrjk104f2l";
      type = "gem";
    };
    version = "1.10.0";
  };
  et-orbi = {
    dependencies = [ "tzinfo" ];
    groups = [ "default" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "0xsfa02hin2ymfcf0bdvsw6wk8w706rrfdqpy6b4f439zbqmn05m";
      type = "gem";
    };
    version = "1.2.6";
  };
  factory_bot = {
    dependencies = [ "activesupport" ];
    groups = [ "default" "development" "test" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "04vxmjr200akcil9fqxc9ghbb9q0lyrh2q03xxncycd5vln910fi";
      type = "gem";
    };
    version = "6.2.0";
  };
  factory_bot_rails = {
    dependencies = [ "factory_bot" "railties" ];
    groups = [ "development" "test" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "18fhcihkc074gk62iwqgbdgc3ymim4fm0b4p3ipffy5hcsb9d2r7";
      type = "gem";
    };
    version = "6.2.0";
  };
  faker = {
    dependencies = [ "i18n" ];
    groups = [ "development" "test" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "1694ndj701a8q4c4bwxz53kx94ih1rr4pgr4gk7a6c8k4jsbjgwi";
      type = "gem";
    };
    version = "2.20.0";
  };
  ffi = {
    groups = [ "default" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "1862ydmclzy1a0cjbvm8dz7847d9rch495ib0zb64y84d3xd4bkg";
      type = "gem";
    };
    version = "1.15.5";
  };
  fugit = {
    dependencies = [ "et-orbi" "raabro" ];
    groups = [ "default" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "0l8878iqg85zmpifjfnidpp17swgh103a0br68nqakflnn0zwcka";
      type = "gem";
    };
    version = "1.5.2";
  };
  globalid = {
    dependencies = [ "activesupport" ];
    groups = [ "default" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "1n5yc058i8xhi1fwcp1w7mfi6xaxfmrifdb4r4hjfff33ldn8lqj";
      type = "gem";
    };
    version = "1.0.0";
  };
  has_scope = {
    dependencies = [ "actionpack" "activesupport" ];
    groups = [ "default" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "00y4jr5fd5lf1b59wxaf5j04afxd4i0sdx6wh51w1ha5njqn7256";
      type = "gem";
    };
    version = "0.8.0";
  };
  i18n = {
    dependencies = [ "concurrent-ruby" ];
    groups = [ "default" "development" "test" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "0b2qyvnk4yynlg17ymkq4g5xgr275637fhl1mjh0valw3cb1fhhg";
      type = "gem";
    };
    version = "1.10.0";
  };
  image_processing = {
    dependencies = [ "mini_magick" "ruby-vips" ];
    groups = [ "default" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "1f32dzj77p9mfp4q95930vfkp80psf88phjc46jhf9ncl72ykffk";
      type = "gem";
    };
    version = "1.12.2";
  };
  io-wait = {
    groups = [ "default" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "10jj6xz15qaw8gkck5wv3a3xg1zdfsarrandkglfbf75x4xmkrfz";
      type = "gem";
    };
    version = "0.2.1";
  };
  jsonapi-renderer = {
    groups = [ "default" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "0ys4drd0k9rw5ixf8n8fx8v0pjh792w4myh0cpdspd317l1lpi5m";
      type = "gem";
    };
    version = "0.2.2";
  };
  loofah = {
    dependencies = [ "crass" "nokogiri" ];
    groups = [ "default" "development" "test" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "0z8bdcmw66j3dy6ivcc02yq32lx3n9bavx497llln8qy014xjm4w";
      type = "gem";
    };
    version = "2.14.0";
  };
  mail = {
    dependencies = [ "mini_mime" ];
    groups = [ "default" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "00wwz6ys0502dpk8xprwcqfwyf3hmnx6lgxaiq6vj43mkx43sapc";
      type = "gem";
    };
    version = "2.7.1";
  };
  marcel = {
    groups = [ "default" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "0kky3yiwagsk8gfbzn3mvl2fxlh3b39v6nawzm4wpjs6xxvvc4x0";
      type = "gem";
    };
    version = "1.0.2";
  };
  method_source = {
    groups = [ "default" "development" "test" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "1pnyh44qycnf9mzi1j6fywd5fkskv3x7nmsqrrws0rjn5dd4ayfp";
      type = "gem";
    };
    version = "1.0.0";
  };
  mini_magick = {
    groups = [ "default" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "1aj604x11d9pksbljh0l38f70b558rhdgji1s9i763hiagvvx2hs";
      type = "gem";
    };
    version = "4.11.0";
  };
  mini_mime = {
    groups = [ "default" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "0lbim375gw2dk6383qirz13hgdmxlan0vc5da2l072j3qw6fqjm5";
      type = "gem";
    };
    version = "1.1.2";
  };
  mini_portile2 = {
    groups = [ "default" "development" "test" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "0rapl1sfmfi3bfr68da4ca16yhc0pp93vjwkj7y3rdqrzy3b41hy";
      type = "gem";
    };
    version = "2.8.0";
  };
  minitest = {
    groups = [ "default" "development" "test" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "06xf558gid4w8lwx13jwfdafsch9maz8m0g85wnfymqj63x5nbbd";
      type = "gem";
    };
    version = "5.15.0";
  };
  mocha = {
    groups = [ "test" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "15s53ggsykk69kxqvs4416s8yxdhz6caggva55n8sjgy4ixzwp10";
      type = "gem";
    };
    version = "1.13.0";
  };
  msgpack = {
    groups = [ "default" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "1cshgsx3hmpgx639xyqjqa2q3hgrhlyr9rpwhsglsx529alqq125";
      type = "gem";
    };
    version = "1.4.5";
  };
  net-imap = {
    dependencies = [ "digest" "net-protocol" "strscan" ];
    groups = [ "default" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "1rl79ykmxa2k4dlk6ykrb9l0a4h101q1gd8c4qv3cl0p9h68zmbn";
      type = "gem";
    };
    version = "0.2.3";
  };
  net-pop = {
    dependencies = [ "digest" "net-protocol" "timeout" ];
    groups = [ "default" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "1slsl3xlbf0cqzmf2q1rfqbm61xvxzmr0h9zprwlbm1xn1cvn9xb";
      type = "gem";
    };
    version = "0.1.1";
  };
  net-protocol = {
    dependencies = [ "io-wait" "timeout" ];
    groups = [ "default" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "0rrzdar609d8c96ikpw2yif44pp5k3n3cpjld31ia0rgmz9z59gv";
      type = "gem";
    };
    version = "0.1.2";
  };
  net-smtp = {
    dependencies = [ "digest" "net-protocol" "timeout" ];
    groups = [ "default" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "1s358kfv9mnfxcjbpr1d5a2gs1q7wkw7ffpn86mf1b3s9p31bw9s";
      type = "gem";
    };
    version = "0.3.1";
  };
  nio4r = {
    groups = [ "default" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "0xk64wghkscs6bv2n22853k2nh39d131c6rfpnlw12mbjnnv9v1v";
      type = "gem";
    };
    version = "2.5.8";
  };
  nokogiri = {
    dependencies = [ "mini_portile2" "racc" ];
    groups = [ "default" "development" "test" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "1p6b3q411h2mw4dsvhjrp1hh66hha5cm69fqg85vn2lizz71n6xz";
      type = "gem";
    };
    version = "1.13.3";
  };
  parallel = {
    groups = [ "default" "development" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "1hkfpm78c2vs1qblnva3k1grijvxh87iixcnyd83s3lxrxsjvag4";
      type = "gem";
    };
    version = "1.21.0";
  };
  parser = {
    dependencies = [ "ast" ];
    groups = [ "default" "development" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "0zaghgvva2q4jqbachg8jvpwgbg3w1jqr0d00m8rqciqznjgsw3c";
      type = "gem";
    };
    version = "3.1.1.0";
  };
  pg = {
    groups = [ "default" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "10ryzmc3r5ja6g90a9ycsxcxsy5872xa1vf01jam0bm74zq3zmi6";
      type = "gem";
    };
    version = "1.3.5";
  };
  puma = {
    dependencies = [ "nio4r" ];
    groups = [ "default" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "0dgr2rybayih2naz3658mbzqwfrg9fxl80zsvhscf6b972kp3jdw";
      type = "gem";
    };
    version = "5.6.4";
  };
  pundit = {
    dependencies = [ "activesupport" ];
    groups = [ "default" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "17z2f7w3syh3c04c8m1v9pvb9pfpymk8b5plszr5l24hx374xvsd";
      type = "gem";
    };
    version = "2.2.0";
  };
  raabro = {
    groups = [ "default" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "10m8bln9d00dwzjil1k42i5r7l82x25ysbi45fwyv4932zsrzynl";
      type = "gem";
    };
    version = "1.4.0";
  };
  racc = {
    groups = [ "default" "development" "test" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "0la56m0z26j3mfn1a9lf2l03qx1xifanndf9p3vx1azf6sqy7v9d";
      type = "gem";
    };
    version = "1.6.0";
  };
  rack = {
    groups = [ "default" "development" "test" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "0i5vs0dph9i5jn8dfc6aqd6njcafmb20rwqngrf759c9cvmyff16";
      type = "gem";
    };
    version = "2.2.3";
  };
  rack-cors = {
    dependencies = [ "rack" ];
    groups = [ "default" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "0jvs0mq8jrsz86jva91mgql16daprpa3qaipzzfvngnnqr5680j7";
      type = "gem";
    };
    version = "1.1.1";
  };
  rack-test = {
    dependencies = [ "rack" ];
    groups = [ "default" "development" "test" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "0rh8h376mx71ci5yklnpqqn118z3bl67nnv5k801qaqn1zs62h8m";
      type = "gem";
    };
    version = "1.1.0";
  };
  rails = {
    dependencies = [ "actioncable" "actionmailbox" "actionmailer" "actionpack" "actiontext" "actionview" "activejob" "activemodel" "activerecord" "activestorage" "activesupport" "railties" ];
    groups = [ "default" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "10x2vb9rd3zpnp6f9vv60kmv9hlrcg4dxk9z5vhcdpkjbh3j8kpf";
      type = "gem";
    };
    version = "7.0.2.3";
  };
  rails-dom-testing = {
    dependencies = [ "activesupport" "nokogiri" ];
    groups = [ "default" "development" "test" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "1lfq2a7kp2x64dzzi5p4cjcbiv62vxh9lyqk2f0rqq3fkzrw8h5i";
      type = "gem";
    };
    version = "2.0.3";
  };
  rails-html-sanitizer = {
    dependencies = [ "loofah" ];
    groups = [ "default" "development" "test" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "09qrfi3pgllxb08r024lln9k0qzxs57v0slsj8616xf9c0cwnwbk";
      type = "gem";
    };
    version = "1.4.2";
  };
  railties = {
    dependencies = [ "actionpack" "activesupport" "method_source" "rake" "thor" "zeitwerk" ];
    groups = [ "default" "development" "test" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "12jxqfalhac6q3v0rasm5r5c406knngrgdblhl41m753gn9hrzx4";
      type = "gem";
    };
    version = "7.0.2.3";
  };
  rainbow = {
    groups = [ "default" "development" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "0smwg4mii0fm38pyb5fddbmrdpifwv22zv3d3px2xx497am93503";
      type = "gem";
    };
    version = "3.1.1";
  };
  rake = {
    groups = [ "default" "development" "test" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "15whn7p9nrkxangbs9hh75q585yfn66lv0v2mhj6q6dl6x8bzr2w";
      type = "gem";
    };
    version = "13.0.6";
  };
  regexp_parser = {
    groups = [ "default" "development" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "155f6cr4rrfw5bs5xd3m5kfw32qhc5fsi4nk82rhif56rc6cs0wm";
      type = "gem";
    };
    version = "2.2.1";
  };
  rexml = {
    groups = [ "default" "development" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "08ximcyfjy94pm1rhcx04ny1vx2sk0x4y185gzn86yfsbzwkng53";
      type = "gem";
    };
    version = "3.2.5";
  };
  rubocop = {
    dependencies = [ "parallel" "parser" "rainbow" "regexp_parser" "rexml" "rubocop-ast" "ruby-progressbar" "unicode-display_width" ];
    groups = [ "default" "development" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "03c6v6bfqdw8vnda0if0sx7aff0iq6xnv1adyfs0bi9msgggafcr";
      type = "gem";
    };
    version = "1.26.0";
  };
  rubocop-ast = {
    dependencies = [ "parser" ];
    groups = [ "default" "development" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "1bd2z82ly7fix8415gvfiwzb6bjialz5rs3sr72kv1lk68rd23wv";
      type = "gem";
    };
    version = "1.16.0";
  };
  rubocop-minitest = {
    dependencies = [ "rubocop" ];
    groups = [ "development" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "0pm8w4z206blrp5ayhzscvaf244dmipmwk2vkaqip3l7xdq9sgf6";
      type = "gem";
    };
    version = "0.18.0";
  };
  rubocop-rails = {
    dependencies = [ "activesupport" "rack" "rubocop" ];
    groups = [ "development" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "14xagb3jbms5mlcf932kx1djn2q4k952d4xia75ll36vh7xf2fpp";
      type = "gem";
    };
    version = "2.14.2";
  };
  ruby-progressbar = {
    groups = [ "default" "development" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "02nmaw7yx9kl7rbaan5pl8x5nn0y4j5954mzrkzi9i3dhsrps4nc";
      type = "gem";
    };
    version = "1.11.0";
  };
  ruby-vips = {
    dependencies = [ "ffi" ];
    groups = [ "default" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "19pzpx406rr9s3qk527rn9y3b76sjq5pi7y0xzqiy50q3k0hhg7g";
      type = "gem";
    };
    version = "2.1.4";
  };
  simplecov = {
    dependencies = [ "docile" "simplecov-html" "simplecov_json_formatter" ];
    groups = [ "test" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "1hrv046jll6ad1s964gsmcq4hvkr3zzr6jc7z1mns22mvfpbc3cr";
      type = "gem";
    };
    version = "0.21.2";
  };
  simplecov-html = {
    groups = [ "default" "test" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "0yx01bxa8pbf9ip4hagqkp5m0mqfnwnw2xk8kjraiywz4lrss6jb";
      type = "gem";
    };
    version = "0.12.3";
  };
  simplecov_json_formatter = {
    groups = [ "default" "test" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "19r15hyvh52jx7fmsrcflb58xh8l7l0zx4sxkh3hqzhq68y81pjl";
      type = "gem";
    };
    version = "0.1.3";
  };
  strscan = {
    groups = [ "default" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "05bzfzfwvs5ngl14yf303nvrg5wjx3srgbjqkki7x65cm66w948p";
      type = "gem";
    };
    version = "3.0.1";
  };
  thor = {
    groups = [ "default" "development" "test" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "0inl77jh4ia03jw3iqm5ipr76ghal3hyjrd6r8zqsswwvi9j2xdi";
      type = "gem";
    };
    version = "1.2.1";
  };
  timeout = {
    groups = [ "default" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "10bx1hcyrjqgq6a848fc1i0cgrvx42gcy8hk4vp90y6zc7k8xzbk";
      type = "gem";
    };
    version = "0.2.0";
  };
  tzinfo = {
    dependencies = [ "concurrent-ruby" ];
    groups = [ "default" "development" "test" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "10qp5x7f9hvlc0psv9gsfbxg4a7s0485wsbq1kljkxq94in91l4z";
      type = "gem";
    };
    version = "2.0.4";
  };
  unicode-display_width = {
    groups = [ "default" "development" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "0csjm9shhfik0ci9mgimb7hf3xgh7nx45rkd9rzgdz6vkwr8rzxn";
      type = "gem";
    };
    version = "2.1.0";
  };
  wahwah = {
    groups = [ "default" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "1kg7jfhr1qphpzxf5dk4ll5gm2h789xkir59zlkwz2pvwfzin02b";
      type = "gem";
    };
    version = "1.3.0";
  };
  websocket-driver = {
    dependencies = [ "websocket-extensions" ];
    groups = [ "default" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "0a3bwxd9v3ghrxzjc4vxmf4xa18c6m4xqy5wb0yk5c6b9psc7052";
      type = "gem";
    };
    version = "0.7.5";
  };
  websocket-extensions = {
    groups = [ "default" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "0hc2g9qps8lmhibl5baa91b4qx8wqw872rgwagml78ydj8qacsqw";
      type = "gem";
    };
    version = "0.1.5";
  };
  will_paginate = {
    groups = [ "default" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "07d7wkxnx0h7dysjny2gw4z5rygdwx1nmjmg5p1x1b9p22wais9l";
      type = "gem";
    };
    version = "3.3.1";
  };
  zeitwerk = {
    groups = [ "default" "development" "test" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "09bq7j2p6mkbxnsg71s253dm2463kg51xc7bmjcxgyblqbh4ln7m";
      type = "gem";
    };
    version = "2.5.4";
  };
}
