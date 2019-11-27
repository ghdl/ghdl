entity tb_ent is
end tb_ent;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_ent is
  signal a : bit_vector(7 downto 0);
begin
  dut: entity work.ent
    port map (a);
end behav;
