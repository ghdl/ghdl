entity tb_repro01 is
end tb_repro01;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_repro01 is
  signal a : std_logic;
  signal b : std_logic;
  signal c : std_logic;
  signal z : std_logic;
begin
  dut: entity work.repro01
    port map (a, b, c, z);

  process
    constant av : std_logic_vector := b"1101";
    constant bv : std_logic_vector := b"0111";
    constant cv : std_logic_vector := b"0011";
    constant zv : std_logic_vector := b"0111";
  begin
    for i in av'range loop
      a <= av (i);
      b <= bv (i);
      c <= cv (i);
      wait for 1 ns;
      assert z = zv(i) severity failure;
    end loop;
    wait;
  end process;
end behav;
