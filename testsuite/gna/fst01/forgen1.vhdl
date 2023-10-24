library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_Std.all;

entity forgen1 is
end;

architecture behav of forgen1 is
  signal top : natural := 123;
begin
  gen: for i in 0 to 3 generate
    signal s : std_logic_vector(i downto 0);
  begin
    process
    begin
      wait for i * 2 ns;
      s <= std_logic_vector(to_unsigned (i, s'length));
      wait;
    end process;
  end generate gen;
end behav;
