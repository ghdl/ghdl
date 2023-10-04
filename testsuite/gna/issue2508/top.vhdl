library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.all;
--use work.compa_pkg.all;


entity	top is

end entity;

architecture behv of top is
signal clk : std_logic := '0';

begin

clk <= not clk after 10 ns;



end architecture;

