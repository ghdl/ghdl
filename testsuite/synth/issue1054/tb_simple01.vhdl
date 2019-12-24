entity tb_simple01 is
end tb_simple01;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_simple01 is
  signal a : std_logic;
  signal b : std_logic;
  signal c : std_logic;
  signal z : std_logic;
begin
  dut: entity work.simple01
    port map (a, b, c, z);

  process
    constant bv : std_logic_vector := b"0111";
    constant cv : std_logic_vector := b"0011";
    constant zv : std_logic_vector := b"0111";
  begin
    a <= '0';
    for i in bv'range loop
      b <= bv (i);
      c <= cv (i);
      wait for 1 ns;
      assert z = zv(i) severity failure;
    end loop;
    wait;
  end process;
end behav;
