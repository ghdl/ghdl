entity tb_ent is
end tb_ent;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_ent is
  signal a : std_logic;
  signal b : std_logic;
  signal z : std_logic;
begin
  dut: entity work.ent
    port map (a, b, z);

  process
    constant av : std_logic_vector := b"1101";
    constant bv : std_logic_vector := b"0111";
    constant zv : std_logic_vector := b"0101";
  begin
    for i in av'range loop
      a <= av (i);
      b <= bv (i);
      wait for 1 ns;
      assert z = zv(i) severity failure;
    end loop;
    wait;
  end process;
end behav;
