library ieee;
use ieee.std_logic_1164.all;

entity ent is
end entity;

architecture a0 of ent is

  signal not_const_signal : std_ulogic_vector(11 downto 0) := x"aaa";

  type byte_array is array (integer range <>) of std_ulogic_vector(7 downto 0);
  constant byte_array_instance : byte_array := (x"00", not_const_signal(7 downto 0) );

begin
end architecture;
