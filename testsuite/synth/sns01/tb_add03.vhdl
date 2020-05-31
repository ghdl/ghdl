entity tb_add03 is
end tb_add03;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_add03 is
  signal a, b : std_logic_vector(8 DOWNTO 0);
  signal borrow : std_logic;
  signal res : std_logic_vector(8 DOWNTO 0);
begin
  dut: entity work.add03
    port map (a, b, borrow, res);

  process
  begin
    a <= b"00000_0100";
    b <= b"00000_0001";
    borrow <= '0';
    wait for 1 ns;
    assert res = b"00000_0011" severity failure;


    a <= b"00000_0010";
    b <= b"00000_0001";
    borrow <= '1';
    wait for 1 ns;
    assert res = b"00000_0000" severity failure;

    wait;
  end process;
end behav;
