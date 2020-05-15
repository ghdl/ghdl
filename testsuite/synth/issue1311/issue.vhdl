library ieee;
use ieee.std_logic_1164.all;

entity issue is
    port (foo : out boolean);
end issue;

architecture beh of issue is
    signal bar : std_logic_vector (7 downto 0);
begin
    foo <= bar (0 downto 1) = bar (1 downto 2);
end architecture beh;
