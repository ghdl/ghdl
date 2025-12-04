library ieee;
use ieee.std_logic_1164.all;

entity concat01 is
end;

architecture behav of concat01 is
begin
  process
    variable s4 : string(1 to 4);
    variable sm : string(2 downto 1);
    variable res : string(1 to 6);
  begin
    s4 := "abcd";
    sm := "ef";
    sm := sm & s4;
    wait;
  end process;
end;

