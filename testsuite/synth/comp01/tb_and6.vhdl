library ieee;
use ieee.std_logic_1164.all;

entity tb_and6 is
end tb_and6;

architecture behav of tb_and6 is
  signal i0, i1, i2, i3, i4, i5 : std_logic;
  signal o : std_logic;

begin
  dut : entity work.and6
    port map (i0 => i0, i1 => i1, i2 => i2, i3 => i4, i4 => i4,
              i5 => i5, o => o);

  process
    constant v0 : std_logic_vector := b"1011";
    constant v1 : std_logic_vector := b"1111";
    constant v2 : std_logic_vector := b"1111";
    constant v3 : std_logic_vector := b"1111";
    constant v4 : std_logic_vector := b"1111";
    constant v5 : std_logic_vector := b"1101";
    constant ov : std_logic_vector := b"1001";
  begin
    for i in ov'range loop
      i0 <= v0 (i);
      i1 <= v1 (i);
      i2 <= v2 (i);
      i3 <= v3 (i);
      i4 <= v4 (i);
      i5 <= v5 (i);
      wait for 1 ns;
      assert o = ov(i) severity failure;
    end loop;
    wait;
  end process;
end behav;
