{ lib, buildGoModule, fetchFromGitHub }:

buildGoModule rec {
  pname = "uni";
  version = "2.4.0";

  src = fetchFromGitHub {
    owner = "arp242";
    repo = "uni";
    rev = "v${version}";
    sha256 = "ZBVUrS1/jUO0iLbK6P5ACw1vS5QPbSMkG9ZTdKrb8eo=";
  };

  vendorSha256 = "+gRESx8KCwfxCztY/68UO8KV+bJmjq3gbsrCFHBZyEI=";

  doCheck = false;

  meta = with lib; {
    homepage = "https://github.com/arp242/uni";
    description = "Query the Unicode database from the commandline, with good support for emojis";
    license = licenses.mit;
    maintainers = with maintainers; [ chvp ];
  };
}
