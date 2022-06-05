entity tb_ivoice2 is
end tb_ivoice2;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_ivoice2 is
  signal romd : std_logic_vector (15 downto 0) := b"1010_0100_0100_0011";
  signal pc : natural range 0 to 7;
  signal res : std_logic_vector (7 downto 0);
begin
  dut: entity work.ivoice2
    port map (pc, romd, res);

  process
  begin
    pc <= 0;
    wait for 1 ns;
    assert res = b"0100_0011" severity failure;

    pc <= 1;
    wait for 1 ns;
    assert res = b"0_0100_001" severity failure;

    pc <= 2;
    wait for 1 ns;
    assert res = b"00_0100_00" severity failure;

    pc <= 3;
    wait for 1 ns;
    assert res = b"100_0100_0" severity failure;

    pc <= 4;
    wait for 1 ns;
    assert res = b"0100_0100" severity failure;

    pc <= 5;
    wait for 1 ns;
    assert res = b"0_0100_010" severity failure;

    pc <= 6;
    wait for 1 ns;
    assert res = b"10_0100_01" severity failure;

    pc <= 7;
    wait for 1 ns;
    assert res = b"010_0100_0" severity failure;

    wait;
  end process;
end behav;
