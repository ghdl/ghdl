library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity arr09 is
  port (val : std_logic_vector(3 downto 0);
        res : out character);
end arr09;

architecture behav of arr09 is
  type map_type is array (natural range 0 to 15) of character;
  constant cmap : map_type := "0123456789abcdef";

  function convert (v : natural range 0 to 15) return character
  is
    variable r : character;
  begin
    r := cmap (v);
    return r;
  end convert;
begin
  res <= convert (3);
end behav;
