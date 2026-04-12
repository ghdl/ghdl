library ieee;
use ieee.std_logic_1164.all;

entity tb_repro1 is
end;

architecture behav of tb_repro1 is
  signal s : std_logic;
begin
 dut: entity work.repro1
   port map (s);

 process
 begin
   wait for 1 ns;
   assert s = '0' severity failure;
   wait;
 end process;
end;
