library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test2 is
end entity;

architecture beh of test2 is
  signal sig  : std_logic_vector(7 downto 0);
begin
  p_proc : process
    constant C_VAL : std_logic_vector(7 downto 0) := (others => '1');
  begin
    sig(C_VAL'range) <= force C_VAL;
    wait;
  end process;
end architecture;
