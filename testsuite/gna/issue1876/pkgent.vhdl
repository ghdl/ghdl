package pkg is
    subtype runner_cfg_t is string;
end;

use work.pkg.runner_cfg_t;

entity pkgent is
  generic ( mygeneric : runner_cfg_t );
end;

architecture arch of pkgent is
begin
  assert mygeneric = "Hello" report "Generic mygeneric is not 'Hello'!" severity failure;
end;
