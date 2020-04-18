library ieee;
use ieee.std_logic_1164.all;

entity issue is
end issue;

architecture beh of issue is
    signal foo  : std_logic_vector (10 downto 0) := (others=>'0');
    signal bar1 : std_logic_vector (10 downto 0) := (others=>'0');
    signal bar2 : std_logic_vector (10 downto 0) := (others=>'0');
begin
    bar1 <= foo or x"40";
    bar2 <= foo or "1";
end architecture beh;

