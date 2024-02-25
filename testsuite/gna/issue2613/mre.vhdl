library ieee;
use ieee.std_logic_1164.all;

entity mre is
  port ( a : out std_logic; b : out std_logic );
end entity;

architecture a of mre is
begin
  a & b <= "01";
end architecture;
