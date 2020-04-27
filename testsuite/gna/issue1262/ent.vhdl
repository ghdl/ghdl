library ieee;
use ieee.std_logic_1164.all;

entity ent is
  generic(
    WIDTH: integer;
    package slv_pkg is new work.slv generic map(N => WIDTH));
  port (
    o_slv: out slv_pkg.slv_t);
end ent;

architecture beh of ent is
  constant ones : std_logic_vector(WIDTH-1 downto 0) := (others => '1');
begin
  o_slv <= ones;
end architecture beh;

