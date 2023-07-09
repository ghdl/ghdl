entity tb_repro1 is
end;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_repro1 is
  signal i, o : std_logic_vector(7 downto 0) := x"01";
begin
  dut: entity work.repro1
    port map (i, o);

  process
  begin
    wait for 1 ns;
    i <= x"01";
    wait for 1 ns;
    i <= x"00";
    wait for 1 ns;
    wait;
  end process;
end behav;
