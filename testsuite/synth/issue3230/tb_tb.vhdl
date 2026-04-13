library ieee;
use ieee.std_logic_1164.all;

entity tb_tb is
end;

architecture behav of tb_tb is
  signal s : std_logic;
begin
 dut: entity work.tb
   port map (s);

 process
 begin
   wait for 1 ns;
   assert s = '1' severity failure;
   wait;
 end process;
end;
