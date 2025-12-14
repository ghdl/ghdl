library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std_unsigned.all;

entity repro is
end;

architecture behav of repro is
begin
  process
    variable v : std_logic_vector(3 downto 0) := x"3";
  begin
    assert v = "00HH" severity failure;
    wait;
  end process;
end behav;
