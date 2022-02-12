{
  actioncable = {
    dependencies = [ "actionpack" "activesupport" "nio4r" "websocket-driver" ];
    groups = [ "default" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "0z3ab9n901craqd3p1yl87kawci0vfw1xlh4d0zkj7lx8hpk10sn";
      type = "gem";
    };
    version = "6.1.4.4";
  };
  actionmailbox = {
    dependencies = [ "actionpack" "activejob" "activerecord" "activestorage" "activesupport" "mail" ];
    groups = [ "default" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "0q94js7ifm0a76xcwxin98bhr8nz0zqcsqi4y7j2mfwm3hq3bh0i";
      type = "gem";
    };
    version = "6.1.4.4";
  };
  actionmailer = {
    dependencies = [ "actionpack" "actionview" "activejob" "activesupport" "mail" "rails-dom-testing" ];
    groups = [ "default" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "1gncnc5xl1ff70mfnqcys2qy65201yjrkwxx0hb5hl7jlamgvz9h";
      type = "gem";
    };
    version = "6.1.4.4";
  };
  actionpack = {
    dependencies = [ "actionview" "activesupport" "rack" "rack-test" "rails-dom-testing" "rails-html-sanitizer" ];
    groups = [ "default" "development" "test" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "171ida68hrk21cq1zz1kfl9h94a3qw5p3afviqzsirl0kx6qjyv9";
      type = "gem";
    };
    version = "6.1.4.4";
  };
  actiontext = {
    dependencies = [ "actionpack" "activerecord" "activestorage" "activesupport" "nokogiri" ];
    groups = [ "default" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "1j9591z8lsp9lx3l75699prw6rgkhhlrfaj4lh5klcdffvxzkvi3";
      type = "gem";
    };
    version = "6.1.4.4";
  };
  actionview = {
    dependencies = [ "activesupport" "builder" "erubi" "rails-dom-testing" "rails-html-sanitizer" ];
    groups = [ "default" "development" "test" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "1lm2pf35p6q4ff78z175h6ihmzfg2j7ssn41374rb9iy9gpiiidm";
      type = "gem";
    };
    version = "6.1.4.4";
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
      sha256 = "0sf0nfjcj1na4v6zaxz6hjglax99yznaymjzpk1fi7mk71qf5hx4";
      type = "gem";
    };
    version = "6.1.4.4";
  };
  activemodel = {
    dependencies = [ "activesupport" ];
    groups = [ "default" "development" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "0g3qdz8dw6zkgz45jd13lwfdnm7rhgczv1pssw63g9k6qj3bkxjm";
      type = "gem";
    };
    version = "6.1.4.4";
  };
  activerecord = {
    dependencies = [ "activemodel" "activesupport" ];
    groups = [ "default" "development" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "090d4wl1pq06m9mibpck0m5nm8h45fwhs3fjx27297kjmnv4gzik";
      type = "gem";
    };
    version = "6.1.4.4";
  };
  activestorage = {
    dependencies = [ "actionpack" "activejob" "activerecord" "activesupport" "marcel" "mini_mime" ];
    groups = [ "default" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "0a6mmm1s8abv11ycqs6cq55kr6j89jpclkcnra9w2k47rl047vk4";
      type = "gem";
    };
    version = "6.1.4.4";
  };
  activesupport = {
    dependencies = [ "concurrent-ruby" "i18n" "minitest" "tzinfo" "zeitwerk" ];
    groups = [ "default" "development" "test" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "0rvnz9lsf9mrkpji748sf51f54m027snkw6rm8flyvf7fq18rm98";
      type = "gem";
    };
    version = "6.1.4.4";
  };
  annotate = {
    dependencies = [ "activerecord" "rake" ];
    groups = [ "development" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "1dxrfppwfg13vqmambbs56xjj8qsdgcy58r2yc44vvy3z1g5yflw";
      type = "gem";
    };
    version = "3.1.1";
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
      sha256 = "02r1c3isfchs5fxivbq99gc3aq4vfyn8snhcy707dal1p8qz12qb";
      type = "gem";
    };
    version = "3.1.16";
  };
  bootsnap = {
    dependencies = [ "msgpack" ];
    groups = [ "default" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "053hx5hz1zdcqwywlnabzf0gkrrchhzh66p1bfzvrryb075lsqm1";
      type = "gem";
    };
    version = "1.10.3";
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
      sha256 = "0hb9wfxyb4ss2vl2mrj1zgdk7dh4yaxghq22gbx62yxj5yb9w4zw";
      type = "gem";
    };
    version = "2.19.0";
  };
  ffi = {
    groups = [ "default" "development" ];
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
      sha256 = "0vdd1kii40qhbr9n8qx71k2gskq6rkl8ygy8hw5hfj8bb5a364xf";
      type = "gem";
    };
    version = "1.8.11";
  };
  image_processing = {
    dependencies = [ "mini_magick" "ruby-vips" ];
    groups = [ "default" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "1778lv4lpwk9ffm0zy7w59gzchq19f22a5gfrdk6qa7l9vx89h11";
      type = "gem";
    };
    version = "1.12.1";
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
  listen = {
    dependencies = [ "rb-fsevent" "rb-inotify" ];
    groups = [ "development" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "0agybr37wpjv3xy4ipcmsvsibgdgphzrwbvcj4vfiykpmakwm01v";
      type = "gem";
    };
    version = "3.7.1";
  };
  loofah = {
    dependencies = [ "crass" "nokogiri" ];
    groups = [ "default" "development" "test" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "17rvbrqcci1579d7dpbsfmz1f9g7msk82lyh9ip5h29dkrnixcgg";
      type = "gem";
    };
    version = "2.13.0";
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
      sha256 = "0d3ga166pahsxavzwj19yjj4lr13rw1vsb36s2qs8blcxigrdp6z";
      type = "gem";
    };
    version = "2.7.1";
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
      sha256 = "0b98w2j7g89ihnc753hh3if68r5qrmdp9n2j6mvqy2yl73sbv739";
      type = "gem";
    };
    version = "1.4.4";
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
      sha256 = "1zqzawia52cdcmi55lp7v8jmiqyw7pcpwsksqlnirwfm3f7bnf11";
      type = "gem";
    };
    version = "1.13.1";
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
      sha256 = "08q20ckhn58m49lccf93p0yv7pkc7hymmcz3di762kb658d5fd38";
      type = "gem";
    };
    version = "3.1.0.0";
  };
  pg = {
    groups = [ "default" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "1zn1miz2h3pki11jwf6f474540kvai3sc43jp8smi3mjx8brvws0";
      type = "gem";
    };
    version = "1.3.1";
  };
  puma = {
    dependencies = [ "nio4r" ];
    groups = [ "default" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "1np2myaxlk5iab1zarwgmp7zsjvm5j8ssg35ijv8b6dpvc3cjd56";
      type = "gem";
    };
    version = "5.6.2";
  };
  pundit = {
    dependencies = [ "activesupport" ];
    groups = [ "default" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "1i3rccbzyw46xrx1ic95r11v15ia2kj8naiyi68gdvxfrisk1fxm";
      type = "gem";
    };
    version = "2.1.1";
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
    dependencies = [ "actioncable" "actionmailbox" "actionmailer" "actionpack" "actiontext" "actionview" "activejob" "activemodel" "activerecord" "activestorage" "activesupport" "railties" "sprockets-rails" ];
    groups = [ "default" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "10vylypjzfp6c34zx175x7ql7h27llmjdhgjxp5bn2zmrx3lac8l";
      type = "gem";
    };
    version = "6.1.4.4";
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
    dependencies = [ "actionpack" "activesupport" "method_source" "rake" "thor" ];
    groups = [ "default" "development" "test" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "1nmyds2www6dmqbbd5ggq31gxxb9mwxd5llzmb3iyczssk6l7lla";
      type = "gem";
    };
    version = "6.1.4.4";
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
  rb-fsevent = {
    groups = [ "default" "development" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "1qsx9c4jr11vr3a9s5j83avczx9qn9rjaf32gxpc2v451hvbc0is";
      type = "gem";
    };
    version = "0.11.0";
  };
  rb-inotify = {
    dependencies = [ "ffi" ];
    groups = [ "default" "development" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "1jm76h8f8hji38z3ggf4bzi8vps6p7sagxn3ab57qc0xyga64005";
      type = "gem";
    };
    version = "0.10.1";
  };
  regexp_parser = {
    groups = [ "default" "development" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "008yn8j44414qxhn1c0nxp4a70rq0bqhz70hnjpgx8cjh2g0makp";
      type = "gem";
    };
    version = "2.2.0";
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
      sha256 = "1l3q96il8zx5zl041lxvmfrndxxpk08fksza1gqshhjjzms7c2rk";
      type = "gem";
    };
    version = "1.25.1";
  };
  rubocop-ast = {
    dependencies = [ "parser" ];
    groups = [ "default" "development" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "1xrij42166a71ixfpfr1pildqdrcmc0cb4906h2s8sk4kqdyngih";
      type = "gem";
    };
    version = "1.15.1";
  };
  rubocop-minitest = {
    dependencies = [ "rubocop" ];
    groups = [ "development" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "0r4iyxxlgin0gy1rzi40lfn3iwl2c9b4dxc493ynbvknl9h1f9qi";
      type = "gem";
    };
    version = "0.17.1";
  };
  rubocop-rails = {
    dependencies = [ "activesupport" "rack" "rubocop" ];
    groups = [ "development" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "0vibx4jhpxaxa8w3wyhasskcgrxfjiyqdwrrc7l2lb0mbxqcs1ly";
      type = "gem";
    };
    version = "2.13.2";
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
      sha256 = "0lk124dixshf8mmrjpsy9avnaygni3cwki25g8nm5py4d2f5fwwa";
      type = "gem";
    };
    version = "2.0.17";
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
  spring = {
    groups = [ "development" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "1x2wz1y2b0kp7mlk9k8zkl39rddk2l3x34b7dar3bh3axd1cs30d";
      type = "gem";
    };
    version = "2.1.1";
  };
  spring-watcher-listen = {
    dependencies = [ "listen" "spring" ];
    groups = [ "development" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "1ybz9nsngfz4psvgnbr3gdk5ibqqhq47lsjkwh5yq4f8brpr10yz";
      type = "gem";
    };
    version = "2.0.1";
  };
  sprockets = {
    dependencies = [ "concurrent-ruby" "rack" ];
    groups = [ "default" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "0ikgwbl6jv3frfiy3xhg5yxw9d0064rgzghar1rg391xmrc4gm38";
      type = "gem";
    };
    version = "4.0.2";
  };
  sprockets-rails = {
    dependencies = [ "actionpack" "activesupport" "sprockets" ];
    groups = [ "default" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "1b9i14qb27zs56hlcc2hf139l0ghbqnjpmfi0054dxycaxvk5min";
      type = "gem";
    };
    version = "3.4.2";
  };
  thor = {
    groups = [ "default" "development" "test" ];
    platforms = [ ];
    source = {
      remotes = [ "https://rubygems.org" ];
      sha256 = "18yhlvmfya23cs3pvhr1qy38y41b6mhr5q9vwv5lrgk16wmf3jna";
      type = "gem";
    };
    version = "1.1.0";
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
      sha256 = "0dk750bhy0l3adnzkkklyima816k7shi54g9byrlw2mq2nnw5s5i";
      type = "gem";
    };
    version = "1.1.1";
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
      sha256 = "0lmg9x683gr9mkrbq9df2m0zb0650mdfxqna0bs10js44inv7znx";
      type = "gem";
    };
    version = "2.5.3";
  };
}
