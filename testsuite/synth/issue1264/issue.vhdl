library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity issue is
    port
        (srl_usn : out unsigned (8-1 downto 0);
         sll_usn : out unsigned (8-1 downto 0);
         srl_sgn : out   signed (8-1 downto 0);
         sll_sgn : out   signed (8-1 downto 0));
end issue;

architecture beh of issue is
begin
    srl_usn <= unsigned'(b"0000_0000") srl 1; -- work
    sll_usn <= unsigned'(b"0000_0000") sll 1; -- fail
    srl_sgn <=   signed'(b"0000_0000") srl 1; -- fail
    sll_sgn <=   signed'(b"0000_0000") sll 1; -- fail
end architecture beh;
