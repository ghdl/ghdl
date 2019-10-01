library ieee;
use ieee.std_logic_1164.all;

entity aggr03 is
  port (a : std_logic_vector (7 downto 0);
        b : out std_logic_vector (7 downto 0));
end aggr03;

architecture rtl of aggr03 is
    signal r : std_logic_vector (7 downto 0);
begin
    b <= r or a;

    r <= (7 downto 2 => '0', others => '1');
end rtl;
