library ieee;
use ieee.std_logic_1164.all;

entity ent2 is
end entity;

architecture a0 of ent2 is

  signal not_const_signal : std_ulogic_vector(11 downto 0) := x"aaa";

  constant cst  : std_ulogic_vector(11 downto 0) := not_const_signal;
begin
end architecture;
