library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity issue is
    port (sub_uns : out unsigned (8-1 downto 0);
          sub_sgn : out   signed (8-1 downto 0));
end issue;

architecture beh of issue is
begin
    sub_uns <= unsigned'(b"0000_0000") - 1; -- works
    sub_sgn <=   signed'(b"0000_0000") - 1; -- fails
end architecture beh;
