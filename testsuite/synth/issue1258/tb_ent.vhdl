entity tb_ent is
end tb_ent;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_ent is
  signal s : std_ulogic;
  signal din : std_ulogic_vector(15 downto 0);
  signal dout : std_ulogic;
begin
  dut: entity work.ent
    port map (s, din, dout);

  process
  begin
    s <= '1';
    din <= x"00_00";
    wait for 1 ns;
    assert dout = '0' severity failure;

    din <= x"04_00";
    wait for 1 ns;
    assert dout = '1' severity failure;

    din <= x"10_40";
    wait for 1 ns;
    assert dout = '0' severity failure;

    din <= x"80_01";
    wait for 1 ns;
    assert dout = '0' severity failure;

    din <= x"80_00";
    wait for 1 ns;
    assert dout = '1' severity failure;

    s <= '0';
    din <= x"80_00";
    wait for 1 ns;
    assert dout = '0' severity failure;

    wait;
  end process;
end behav;
