library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ent is
end entity;

architecture a of ent is
  procedure proc(constant value : std_logic_vector) is
  begin
  end procedure;
begin
  main : process is
    constant const : unsigned(1 downto 0) := "00";
  begin
    proc(std_logic_vector(const));
    wait;
  end process;
end architecture;

