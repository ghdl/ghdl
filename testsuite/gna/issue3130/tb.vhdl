package generic_pkg is
   generic (g_dummy : integer);
   type t is array (natural range 0 to 0) of integer;
end package;

package instance_pkg is
   package pkg is new work.generic_pkg generic map(0);
end package;

entity sub_entity is
   generic (
      g_natural : natural;
      type g_type
   );
end entity;
architecture arch of sub_entity is
begin
end;

library ieee;
use ieee.std_logic_1164.all;

entity baz is
   generic (g_natural :     natural);
   port (foo          : out std_logic_vector(g_natural - 1 downto 0));
end entity;

-- use work.instance_pkg.pkg.t; -- should this be necessary?

architecture arch of baz is
   signal bar : std_logic_vector(g_natural - 1 downto 0);
begin
   foo <= bar;
   i_sub_entity : entity work.sub_entity
      generic map(
         g_natural => 35,
         g_type    => work.instance_pkg.pkg.t
      );
end architecture;

entity tb is
end entity;

architecture arch of tb is
begin
   i_uut : entity work.baz
      generic map(g_natural => 42)
      port map(foo          => open);
end architecture;
