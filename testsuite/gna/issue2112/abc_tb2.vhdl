library ieee;
use ieee.std_logic_1164.all;

entity abc_tb2 is
end;

architecture sim of abc_tb2 is
  type Indices_t is array (natural range <>) of std_logic_vector;

  constant c : Indices_t := (0 => std_logic_vector'(x"00"),
                             1 => std_logic_vector'(x"01"));
begin
end architecture sim;
