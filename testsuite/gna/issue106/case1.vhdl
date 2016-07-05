library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ent is
end entity ent;

architecture arch of ent is
  signal test: natural;
  constant e : natural := 3;
begin
  LL: case e generate
    when 0 =>
    when 1 to 4 =>
       test <= 1;
    when l5: 5 | 7=>
       test <= 2;
    when others =>
  end generate ll;

  process
  begin
    wait for 0 ns;
    assert test = 2;
    wait;
  end process;
end architecture arch;


