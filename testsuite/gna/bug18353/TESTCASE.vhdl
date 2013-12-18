library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


entity TESTCASE is
end TESTCASE;


architecture DUMMY of TESTCASE is
begin

  dummy: process
    constant str : string := "8#5382#";
    variable xv  : integer;
  begin
    xv := integer'value(str);
    report "xv := " & integer'image(xv) severity NOTE;
    wait;
  end process;

end DUMMY;
