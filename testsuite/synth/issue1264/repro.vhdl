library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity repro is
    port (r : out std_logic);
end repro;

architecture beh of repro is
begin
  r <= '1';

  assert (unsigned'(b"1001_0001") srl 1) = b"0100_1000";
  assert (unsigned'(b"1001_0001") sll 1) = b"0010_0010";
  assert (signed'(b"1001_0001") srl 1) = b"0100_1000";
  assert (signed'(b"1001_0001") sll 1) = b"0010_0010";

  --  assert false report to_bstring(signed'(b"1001_0001") srl 1);
end architecture beh;
