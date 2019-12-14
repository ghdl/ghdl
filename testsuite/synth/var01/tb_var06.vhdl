entity tb_var06 is
end tb_var06;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_var06 is
  signal clk : std_logic;
  signal mask : std_logic_vector (1 downto 0);
  signal val : std_logic_vector (15 downto 0);
  signal res : std_logic_vector (15 downto 0);
begin
  dut: entity work.var06
    port map (
      mask => mask,
      val => val,
      res => res);

  process
  begin
    mask <= "11";
    val <= x"aa_bb";
    wait for 1 ns;
    assert res = x"aa_bb" severity failure;

    mask <= "00";
    val <= x"12_34";
    wait for 1 ns;
    assert res = x"00_00" severity failure;

    mask <= "10";
    val <= x"12_34";
    wait for 1 ns;
    assert res = x"12_00" severity failure;

    wait;
  end process;
end behav;
