library ieee;
use ieee.std_logic_1164.all;

entity repro3 is
  generic (
     word_len : natural := 20);
end;

architecture behav of repro3 is
  subtype word is std_logic_vector(word_len - 1 downto 0);
  
  function F return word is
  begin
    return ("1010","101010101010", others => '0');
  end function;
begin
  assert f = b"1010_101010101010_0000" severity failure;
  process
  begin
    report to_string(f);
    wait;
  end process;
end;

