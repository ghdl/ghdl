package generic_pkg is
   generic (type g_type);
end package;

use std.textio.all;

entity repro_err is
end entity;

architecture arch of repro_err is
  package my_pkg is new work.generic_pkg generic map (g_type => text);
begin
end architecture;
