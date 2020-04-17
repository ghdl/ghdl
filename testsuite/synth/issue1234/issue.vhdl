library ieee;
use ieee.std_logic_1164.all;

entity issue is
    port (foobar : out std_logic_vector (3 downto 0));
end issue;

architecture beh of issue is
begin
    foobar <= ((foobar'range=>'0'));
end architecture;
