
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb_test is end;

architecture arch_tb of tb_test is
--   signal reset_s, clk_s : std_logic;
   signal i_s : integer := -1;
--   signal j_s : integer := -2;
   -- Here, as it should, an error will be raised during compilation
--   signal u_s : unsigned(7 downto 0) := to_unsigned(-1, 8);
   --
   signal v_s : unsigned(7 downto 0);
--   signal w_s : unsigned(7 downto 0);
begin
   -- Here, as it should, a bound check failure will be raised during simulation
--   w_s <= to_unsigned(j_s, 8);
   --
   -- Here it won't have any error during simulation, but it should
   v_s <= to_unsigned(i_s, 8);
   --
end architecture arch_tb;
