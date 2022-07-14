entity tb_latch01 is
end tb_latch01;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_latch01 is
  signal q : std_logic;
  signal d : std_logic;
  signal en : std_logic;
begin
  dut: entity work.latch01
    port map (q, d, en);

  process
    constant dv : std_logic_vector := b"010011";
    constant ev : std_logic_vector := b"110101";
    constant qv : std_logic_vector := b"011001";
  begin
    for i in dv'range loop
      d <= dv (i);
      en <= ev (i);
      wait for 1 ns;
      assert q = qv(i) severity failure;
    end loop;
    wait;
  end process;
end behav;
