{
  actioncable = {
    dependencies = ["actionpack" "activesupport" "nio4r" "websocket-driver"];
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1zmfy7gqkjxkxfza29x6aizg5kqwfl0kx6ii5rqd1xd2sw26s5dw";
      type = "gem";
    };
    version = "6.1.1";
  };
  actionmailbox = {
    dependencies = ["actionpack" "activejob" "activerecord" "activestorage" "activesupport" "mail"];
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1yp26w3y7z3rlgwqdhc66rjzq7v2d8ymjhizqlj3i8iygag6l51l";
      type = "gem";
    };
    version = "6.1.1";
  };
  actionmailer = {
    dependencies = ["actionpack" "actionview" "activejob" "activesupport" "mail" "rails-dom-testing"];
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "05wg2q3crdyr5kz6fjv3xa35l6wy0f8nn0ngip1r3mr23gb98hww";
      type = "gem";
    };
    version = "6.1.1";
  };
  actionpack = {
    dependencies = ["actionview" "activesupport" "rack" "rack-test" "rails-dom-testing" "rails-html-sanitizer"];
    groups = ["default" "development" "test"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1qsk26mivldbab7gv013p21b90vpbh4484kpvgkakr1i3sqy7kry";
      type = "gem";
    };
    version = "6.1.1";
  };
  actiontext = {
    dependencies = ["actionpack" "activerecord" "activestorage" "activesupport" "nokogiri"];
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0v4rdk7dmprykjm4490rl01vja44ipmrd39mlh5wy559zi2rhx3f";
      type = "gem";
    };
    version = "6.1.1";
  };
  actionview = {
    dependencies = ["activesupport" "builder" "erubi" "rails-dom-testing" "rails-html-sanitizer"];
    groups = ["default" "development" "test"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "17vyb7ccs2zgazm3pig6dal4z962491pz7085n102rp62lm6r7nw";
      type = "gem";
    };
    version = "6.1.1";
  };
  active_model_serializers = {
    dependencies = ["actionpack" "activemodel" "case_transform" "jsonapi-renderer"];
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1jjkmrx82rn0d2bhpi6kz2h1s4w7rpk5pj2vcssyc1a2ys12vyhj";
      type = "gem";
    };
    version = "0.10.12";
  };
  activejob = {
    dependencies = ["activesupport" "globalid"];
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "12d109axac33h090cr3q7yyg0ihlcim8866fmrq3pxfr95mh4ppk";
      type = "gem";
    };
    version = "6.1.1";
  };
  activemodel = {
    dependencies = ["activesupport"];
    groups = ["default" "development"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0byi7yv3zm2b1bi2i424phf5sd1q7i5682f0d1ly75v08iax9fdj";
      type = "gem";
    };
    version = "6.1.1";
  };
  activerecord = {
    dependencies = ["activemodel" "activesupport"];
    groups = ["default" "development"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1np967p465ydls2c2qwsnh7bhijw7l6mjirbnjkj8fqvm73s43yi";
      type = "gem";
    };
    version = "6.1.1";
  };
  activestorage = {
    dependencies = ["actionpack" "activejob" "activerecord" "activesupport" "marcel" "mimemagic"];
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1f54a589x0hdrmykxyidz4wns8ggcm598fcbbsvag1glnc32i2vr";
      type = "gem";
    };
    version = "6.1.1";
  };
  activesupport = {
    dependencies = ["concurrent-ruby" "i18n" "minitest" "tzinfo" "zeitwerk"];
    groups = ["default" "development" "test"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "08pahpg5444lv5arqxnh6x8mdayfn7jh464k7ywypfxjhqaxkmx1";
      type = "gem";
    };
    version = "6.1.1";
  };
  annotate = {
    dependencies = ["activerecord" "rake"];
    groups = ["development"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1dxrfppwfg13vqmambbs56xjj8qsdgcy58r2yc44vvy3z1g5yflw";
      type = "gem";
    };
    version = "3.1.1";
  };
  ast = {
    groups = ["default" "development"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1l3468czzjmxl93ap40hp7z94yxp4nbag0bxqs789bm30md90m2a";
      type = "gem";
    };
    version = "2.4.1";
  };
  bcrypt = {
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "02r1c3isfchs5fxivbq99gc3aq4vfyn8snhcy707dal1p8qz12qb";
      type = "gem";
    };
    version = "3.1.16";
  };
  bootsnap = {
    dependencies = ["msgpack"];
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1qx1f729bgh391agsqb4ngzn22wdn4cc6mkp0cipf0d5hsg9cpaq";
      type = "gem";
    };
    version = "1.5.1";
  };
  builder = {
    groups = ["default" "development" "test"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "045wzckxpwcqzrjr353cxnyaxgf0qg22jh00dcx7z38cys5g1jlr";
      type = "gem";
    };
    version = "3.2.4";
  };
  byebug = {
    groups = ["development" "test"];
    platforms = [{
      engine = "maglev";
    } {
      engine = "mingw";
    } {
      engine = "mingw";
    } {
      engine = "ruby";
    }];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0nx3yjf4xzdgb8jkmk2344081gqr22pgjqnmjg2q64mj5d6r9194";
      type = "gem";
    };
    version = "11.1.3";
  };
  case_transform = {
    dependencies = ["activesupport"];
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0fzyws6spn5arqf6q604dh9mrj84a36k5hsc8z7jgcpfvhc49bg2";
      type = "gem";
    };
    version = "0.2";
  };
  codecov = {
    dependencies = ["simplecov"];
    groups = ["test"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1a2yraa3fyvpjx56ymgzc7vzr8g3m1aqgm60svam16yqplg1b3wr";
      type = "gem";
    };
    version = "0.3.0";
  };
  concurrent-ruby = {
    groups = ["default" "development" "test"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1vnxrbhi7cq3p4y2v9iwd10v1c7l15is4var14hwnb2jip4fyjzz";
      type = "gem";
    };
    version = "1.1.7";
  };
  crass = {
    groups = ["default" "development" "test"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0pfl5c0pyqaparxaqxi6s4gfl21bdldwiawrc0aknyvflli60lfw";
      type = "gem";
    };
    version = "1.0.6";
  };
  delayed_cron_job = {
    dependencies = ["delayed_job"];
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1qsf7csnhyk787yx88ilsqris3h0gga3g6ri31hccdfbdab1f33a";
      type = "gem";
    };
    version = "0.7.4";
  };
  delayed_job = {
    dependencies = ["activesupport"];
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "19ym3jw2jj1pxm6p22x2mpf69sdxiw07ddr69v92ccgg6d7q87rh";
      type = "gem";
    };
    version = "4.1.9";
  };
  delayed_job_active_record = {
    dependencies = ["activerecord" "delayed_job"];
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1d29cy14chn1dqck5z15rxsmah289m9yr7wa8c9k8bx15ar7pf3g";
      type = "gem";
    };
    version = "4.1.5";
  };
  docile = {
    groups = ["default" "test"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1bpdrsdqwv80qqc3f4xxzpii13lx9mlx3zay4bnmmscrx8c0p63z";
      type = "gem";
    };
    version = "1.3.5";
  };
  erubi = {
    groups = ["default" "development" "test"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "09l8lz3j00m898li0yfsnb6ihc63rdvhw3k5xczna5zrjk104f2l";
      type = "gem";
    };
    version = "1.10.0";
  };
  factory_bot = {
    dependencies = ["activesupport"];
    groups = ["default" "development" "test"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "11ij9s4hasy963qjqbrrf0m8lm9m9pxkh2vf4wrnafa6gw6r9qk8";
      type = "gem";
    };
    version = "6.1.0";
  };
  factory_bot_rails = {
    dependencies = ["factory_bot" "railties"];
    groups = ["development" "test"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0hfxkq6rarg0b8xfzqg200xyj176sn1xplqqqcrz5drhkqp30m14";
      type = "gem";
    };
    version = "6.1.0";
  };
  faker = {
    dependencies = ["i18n"];
    groups = ["development" "test"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1l0dvswigzxaz9558wmfix3v8cmwwkgdvrx1fmpd3qnr5hky1qrk";
      type = "gem";
    };
    version = "2.15.1";
  };
  ffi = {
    groups = ["default" "development"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "15hgiy09i8ywjihyzyvjvk42ivi3kmy6dm21s5sgg9j7y3h3zkkx";
      type = "gem";
    };
    version = "1.14.2";
  };
  globalid = {
    dependencies = ["activesupport"];
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1zkxndvck72bfw235bd9nl2ii0lvs5z88q14706cmn702ww2mxv1";
      type = "gem";
    };
    version = "0.4.2";
  };
  has_scope = {
    dependencies = ["actionpack" "activesupport"];
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "14df3am60hmyadjz2wah1qny2l5as51gvlxd0nycygmiqbfa0kyg";
      type = "gem";
    };
    version = "0.7.2";
  };
  i18n = {
    dependencies = ["concurrent-ruby"];
    groups = ["default" "development" "test"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1kr0bx9323fv5ys6nlhsy05kmwcbs94h6ac7ka9qqywy0vbdvrrv";
      type = "gem";
    };
    version = "1.8.7";
  };
  image_processing = {
    dependencies = ["mini_magick" "ruby-vips"];
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1778lv4lpwk9ffm0zy7w59gzchq19f22a5gfrdk6qa7l9vx89h11";
      type = "gem";
    };
    version = "1.12.1";
  };
  jsonapi-renderer = {
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0ys4drd0k9rw5ixf8n8fx8v0pjh792w4myh0cpdspd317l1lpi5m";
      type = "gem";
    };
    version = "0.2.2";
  };
  listen = {
    dependencies = ["rb-fsevent" "rb-inotify"];
    groups = ["development"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0imzd0cb9vlkc3yggl4rph1v1wm4z9psgs4z6aqsqa5hgf8gr9hj";
      type = "gem";
    };
    version = "3.4.1";
  };
  loofah = {
    dependencies = ["crass" "nokogiri"];
    groups = ["default" "development" "test"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0ndimir6k3kfrh8qrb7ir1j836l4r3qlwyclwjh88b86clblhszh";
      type = "gem";
    };
    version = "2.8.0";
  };
  mail = {
    dependencies = ["mini_mime"];
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "00wwz6ys0502dpk8xprwcqfwyf3hmnx6lgxaiq6vj43mkx43sapc";
      type = "gem";
    };
    version = "2.7.1";
  };
  marcel = {
    dependencies = ["mimemagic"];
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1nxbjmcyg8vlw6zwagf17l9y2mwkagmmkg95xybpn4bmf3rfnksx";
      type = "gem";
    };
    version = "0.3.3";
  };
  method_source = {
    groups = ["default" "development" "test"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1pnyh44qycnf9mzi1j6fywd5fkskv3x7nmsqrrws0rjn5dd4ayfp";
      type = "gem";
    };
    version = "1.0.0";
  };
  mimemagic = {
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1qfqb9w76kmpb48frbzbyvjc0dfxh5qiw1kxdbv2y2kp6fxpa1kf";
      type = "gem";
    };
    version = "0.3.5";
  };
  mini_magick = {
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1aj604x11d9pksbljh0l38f70b558rhdgji1s9i763hiagvvx2hs";
      type = "gem";
    };
    version = "4.11.0";
  };
  mini_mime = {
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1axm0rxyx3ss93wbmfkm78a6x03l8y4qy60rhkkiq0aza0vwq3ha";
      type = "gem";
    };
    version = "1.0.2";
  };
  mini_portile2 = {
    groups = ["default" "development" "test"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1hdbpmamx8js53yk3h8cqy12kgv6ca06k0c9n3pxh6b6cjfs19x7";
      type = "gem";
    };
    version = "2.5.0";
  };
  minitest = {
    groups = ["default" "development" "test"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0ipjhdw8ds6q9h7bs3iw28bjrwkwp215hr4l3xf6215fsl80ky5j";
      type = "gem";
    };
    version = "5.14.3";
  };
  mocha = {
    groups = ["test"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "05yw6rwgjppq116jgqfg4pv4bql3ci4r2fmmg0m2c3sqib1bq41a";
      type = "gem";
    };
    version = "1.12.0";
  };
  msgpack = {
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1lva6bkvb4mfa0m3bqn4lm4s4gi81c40jvdcsrxr6vng49q9daih";
      type = "gem";
    };
    version = "1.3.3";
  };
  nio4r = {
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1cbwp1kbv6b2qfxv8sarv0d0ilb257jihlvdqj8f5pdm0ksq1sgk";
      type = "gem";
    };
    version = "2.5.4";
  };
  nokogiri = {
    dependencies = ["mini_portile2" "racc"];
    groups = ["default" "development" "test"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1ajwkqr28hwqbyl1l3czx4a34c88acxywyqp8cjyy0zgsd6sbhj2";
      type = "gem";
    };
    version = "1.11.1";
  };
  parallel = {
    groups = ["default" "development"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0055br0mibnqz0j8wvy20zry548dhkakws681bhj3ycb972awkzd";
      type = "gem";
    };
    version = "1.20.1";
  };
  parser = {
    dependencies = ["ast"];
    groups = ["default" "development"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1jixakyzmy0j5c1rb0fjrrdhgnyryvrr6vgcybs14jfw09akv5ml";
      type = "gem";
    };
    version = "3.0.0.0";
  };
  pg = {
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "13mfrysrdrh8cka1d96zm0lnfs59i5x2g6ps49r2kz5p3q81xrzj";
      type = "gem";
    };
    version = "1.2.3";
  };
  puma = {
    dependencies = ["nio4r"];
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "13640p5fk19705ygp8j6p07lccag3d80bx8bmjgpd5zsxxsdc50b";
      type = "gem";
    };
    version = "5.1.1";
  };
  pundit = {
    dependencies = ["activesupport"];
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "18kwm5rkazb89yf792y3fxqihcxw2vdy7k1w542s4hg82ibfpyx3";
      type = "gem";
    };
    version = "2.1.0";
  };
  racc = {
    groups = ["default" "development" "test"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "178k7r0xn689spviqzhvazzvxfq6fyjldxb3ywjbgipbfi4s8j1g";
      type = "gem";
    };
    version = "1.5.2";
  };
  rack = {
    groups = ["default" "development" "test"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1x72abm2snhk664s9g642pxjyk911y39qck8n9hbnqmb7ba1zbmz";
      type = "gem";
    };
    version = "2.1.4";
  };
  rack-cors = {
    dependencies = ["rack"];
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0jvs0mq8jrsz86jva91mgql16daprpa3qaipzzfvngnnqr5680j7";
      type = "gem";
    };
    version = "1.1.1";
  };
  rack-test = {
    dependencies = ["rack"];
    groups = ["default" "development" "test"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0rh8h376mx71ci5yklnpqqn118z3bl67nnv5k801qaqn1zs62h8m";
      type = "gem";
    };
    version = "1.1.0";
  };
  rails = {
    dependencies = ["actioncable" "actionmailbox" "actionmailer" "actionpack" "actiontext" "actionview" "activejob" "activemodel" "activerecord" "activestorage" "activesupport" "railties" "sprockets-rails"];
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0r2ahd5rc0i67jgnamb2qa8zps1pfkbg63x3zyqjvxwaws10ywdp";
      type = "gem";
    };
    version = "6.1.1";
  };
  rails-dom-testing = {
    dependencies = ["activesupport" "nokogiri"];
    groups = ["default" "development" "test"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1lfq2a7kp2x64dzzi5p4cjcbiv62vxh9lyqk2f0rqq3fkzrw8h5i";
      type = "gem";
    };
    version = "2.0.3";
  };
  rails-html-sanitizer = {
    dependencies = ["loofah"];
    groups = ["default" "development" "test"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1icpqmxbppl4ynzmn6dx7wdil5hhq6fz707m9ya6d86c7ys8sd4f";
      type = "gem";
    };
    version = "1.3.0";
  };
  railties = {
    dependencies = ["actionpack" "activesupport" "method_source" "rake" "thor"];
    groups = ["default" "development" "test"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1ic1rvvkfj59v4xj4ajczhqggy008f98wgb7zkhjvcjmfx2ih27s";
      type = "gem";
    };
    version = "6.1.1";
  };
  rainbow = {
    groups = ["default" "development"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0bb2fpjspydr6x0s8pn1pqkzmxszvkfapv0p4627mywl7ky4zkhk";
      type = "gem";
    };
    version = "3.0.0";
  };
  rake = {
    groups = ["default" "development" "test"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1iik52mf9ky4cgs38fp2m8r6skdkq1yz23vh18lk95fhbcxb6a67";
      type = "gem";
    };
    version = "13.0.3";
  };
  rb-fsevent = {
    groups = ["default" "development"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1k9bsj7ni0g2fd7scyyy1sk9dy2pg9akniahab0iznvjmhn54h87";
      type = "gem";
    };
    version = "0.10.4";
  };
  rb-inotify = {
    dependencies = ["ffi"];
    groups = ["default" "development"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1jm76h8f8hji38z3ggf4bzi8vps6p7sagxn3ab57qc0xyga64005";
      type = "gem";
    };
    version = "0.10.1";
  };
  regexp_parser = {
    groups = ["default" "development"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0zm86k9q8m5jkcnpb1f93wsvc57saldfj8czxkx1aw031i95inip";
      type = "gem";
    };
    version = "2.0.3";
  };
  rexml = {
    groups = ["default" "development"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1mkvkcw9fhpaizrhca0pdgjcrbns48rlz4g6lavl5gjjq3rk2sq3";
      type = "gem";
    };
    version = "3.2.4";
  };
  rubocop = {
    dependencies = ["parallel" "parser" "rainbow" "regexp_parser" "rexml" "rubocop-ast" "ruby-progressbar" "unicode-display_width"];
    groups = ["default" "development"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1682ca0yjlc2w66qyskh3p6yb0drxw5k3xnndba3zgcqlfq5iz5z";
      type = "gem";
    };
    version = "1.8.1";
  };
  rubocop-ast = {
    dependencies = ["parser"];
    groups = ["default" "development"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1qvfp567aprjgcwj757p55ynj0dx2b3c3hd76za9z3c43sphprcj";
      type = "gem";
    };
    version = "1.4.0";
  };
  rubocop-minitest = {
    dependencies = ["rubocop"];
    groups = ["development"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0nzwx08x48ij2bmndpp34hzhkd3g2qd5z7jzbz6phmafq6gp6530";
      type = "gem";
    };
    version = "0.10.3";
  };
  rubocop-rails = {
    dependencies = ["activesupport" "rack" "rubocop"];
    groups = ["development"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0h656la1g644g54g3gidz45p6v8i1156nw6bi66cfx7078y1339d";
      type = "gem";
    };
    version = "2.9.1";
  };
  ruby-progressbar = {
    groups = ["default" "development"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "02nmaw7yx9kl7rbaan5pl8x5nn0y4j5954mzrkzi9i3dhsrps4nc";
      type = "gem";
    };
    version = "1.11.0";
  };
  ruby-vips = {
    dependencies = ["ffi"];
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0lk124dixshf8mmrjpsy9avnaygni3cwki25g8nm5py4d2f5fwwa";
      type = "gem";
    };
    version = "2.0.17";
  };
  simplecov = {
    dependencies = ["docile" "simplecov-html" "simplecov_json_formatter"];
    groups = ["test"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1mm20dvd64w46l5k11il9z5sjgdpp0bknml76glcngvl2w03k3cb";
      type = "gem";
    };
    version = "0.20.0";
  };
  simplecov-html = {
    groups = ["default" "test"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0yx01bxa8pbf9ip4hagqkp5m0mqfnwnw2xk8kjraiywz4lrss6jb";
      type = "gem";
    };
    version = "0.12.3";
  };
  simplecov_json_formatter = {
    groups = ["default" "test"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0cl3j7p3b5q7sxsx1va63c8imc5x6g99xablz08qrmqhpi0d6g6j";
      type = "gem";
    };
    version = "0.1.2";
  };
  spring = {
    groups = ["development"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1x2wz1y2b0kp7mlk9k8zkl39rddk2l3x34b7dar3bh3axd1cs30d";
      type = "gem";
    };
    version = "2.1.1";
  };
  spring-watcher-listen = {
    dependencies = ["listen" "spring"];
    groups = ["development"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1ybz9nsngfz4psvgnbr3gdk5ibqqhq47lsjkwh5yq4f8brpr10yz";
      type = "gem";
    };
    version = "2.0.1";
  };
  sprockets = {
    dependencies = ["concurrent-ruby" "rack"];
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0ikgwbl6jv3frfiy3xhg5yxw9d0064rgzghar1rg391xmrc4gm38";
      type = "gem";
    };
    version = "4.0.2";
  };
  sprockets-rails = {
    dependencies = ["actionpack" "activesupport" "sprockets"];
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0mwmz36265646xqfyczgr1mhkm1hfxgxxvgdgr4xfcbf2g72p1k2";
      type = "gem";
    };
    version = "3.2.2";
  };
  thor = {
    groups = ["default" "development" "test"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1xbhkmyhlxwzshaqa7swy2bx6vd64mm0wrr8g3jywvxy7hg0cwkm";
      type = "gem";
    };
    version = "1.0.1";
  };
  tzinfo = {
    dependencies = ["concurrent-ruby"];
    groups = ["default" "development" "test"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "10qp5x7f9hvlc0psv9gsfbxg4a7s0485wsbq1kljkxq94in91l4z";
      type = "gem";
    };
    version = "2.0.4";
  };
  unicode-display_width = {
    groups = ["default" "development"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1bilbnc8j6jkb59lrf177i3p1pdyxll0n8400hzqr35vl3r3kv2m";
      type = "gem";
    };
    version = "2.0.0";
  };
  wahwah = {
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0dk750bhy0l3adnzkkklyima816k7shi54g9byrlw2mq2nnw5s5i";
      type = "gem";
    };
    version = "1.1.1";
  };
  websocket-driver = {
    dependencies = ["websocket-extensions"];
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1i3rs4kcj0jba8idxla3s6xd1xfln3k8b4cb1dik2lda3ifnp3dh";
      type = "gem";
    };
    version = "0.7.3";
  };
  websocket-extensions = {
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0hc2g9qps8lmhibl5baa91b4qx8wqw872rgwagml78ydj8qacsqw";
      type = "gem";
    };
    version = "0.1.5";
  };
  will_paginate = {
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "10qk4mf3rfc0vr26j0ba6vcz7407rdjfn13ph690pkzr94rv8bay";
      type = "gem";
    };
    version = "3.3.0";
  };
  zeitwerk = {
    groups = ["default" "development" "test"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1746czsjarixq0x05f7p3hpzi38ldg6wxnxxw74kbjzh1sdjgmpl";
      type = "gem";
    };
    version = "2.4.2";
  };
}
