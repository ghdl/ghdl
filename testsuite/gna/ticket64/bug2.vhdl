library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ent2 is
end entity;

architecture a of ent2 is
  procedure proc(constant value : std_logic_vector) is
    constant l : natural := maximum (value'length, value'length);
  begin
  end procedure;
begin
end architecture;

