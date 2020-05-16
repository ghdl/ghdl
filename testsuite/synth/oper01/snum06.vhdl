entity snum06 is
end snum06;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture behav of snum06 is
begin
  assert signed'("0100") * signed'("001") = signed'("0000100");
  assert signed'("0101") * signed'("001") = signed'("0000101");
  assert signed'("0101") * signed'("011") = signed'("0001111");
  assert signed'("11") * signed'("11") = signed'("0001");
  assert signed'("10") * signed'("11") = signed'("0010");
  assert signed'("10") * signed'("01") = signed'("1110");
  assert signed'("01") * signed'("10") = signed'("1110");

  assert unsigned'("1011") * unsigned'("000011") = unsigned'("0000100001");
  --  assert false report to_bstring(unsigned'("1011") * unsigned'("000011"));
end behav;
