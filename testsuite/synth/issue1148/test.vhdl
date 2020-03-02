library IEEE;
use IEEE.std_logic_1164.all;
use std.textio.all;

entity mwe is
end mwe;

architecture test of mwe is

begin

  process
    variable starttime : time := 1120 us;
    variable endtime : time := 2031 us;
    variable dt : time;

  begin
    dt := endtime - starttime;
    report "Resulting frequency is: " & to_string(1.0 / (dt / 1000 ms)) & " Hz";

    wait;
  end process;
end test;
