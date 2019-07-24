library ieee;
use ieee.std_logic_1164.all;

entity case02 is
  port (a : std_logic_vector (4 downto 0);
        o : out std_logic);
end case02;

architecture behav of case02 is
begin
  with a select o <=
    '1' when "00011",
    '1' when "00110" | "00111" | "10001",
    '0' when "00100",
    '1' when "01100",
    '1' when "10000",
    '0' when others;
end behav;
