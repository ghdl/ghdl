library ieee;
use ieee.std_logic_1164.all;

entity tb_and3 is
end tb_and3;

architecture behav of tb_and3 is
  signal i0, i1, i2 : std_logic;
  signal o : std_logic;

begin
  dut : entity work.and3
    port map (i0 => i0, i1 => i1, i2 => i2, o => o);

  process
    constant v0 : std_logic_vector := b"1011";
    constant v1 : std_logic_vector := b"1111";
    constant v2 : std_logic_vector := b"1101";
    constant ov : std_logic_vector := b"1001";
  begin
    for i in ov'range loop
      i0 <= v0 (i);
      i1 <= v1 (i);
      i2 <= v2 (i);
      wait for 1 ns;
      assert o = ov(i) severity failure;
    end loop;
    wait;
  end process;
end behav;
