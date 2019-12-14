entity tb_var03 is
end tb_var03;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_var03 is
  signal clk : std_logic;
  signal mask : std_logic_vector (1 downto 0);
  signal a, b : std_logic_vector (15 downto 0);
  signal res : std_logic_vector (15 downto 0);
begin
  dut: entity work.var03
    port map (
      mask => mask,
      a => a,
      b => b,
      res => res);

  process
  begin
    mask <= "11";
    a <= x"12_34";
    b <= x"aa_bb";
    wait for 1 ns;
    assert res = x"aa_bb" severity failure;

    mask <= "00";
    a <= x"aa_bb";
    b <= x"12_34";
    wait for 1 ns;
    assert res = x"aa_bb" severity failure;

    mask <= "10";
    a <= x"aa_bb";
    b <= x"12_34";
    wait for 1 ns;
    assert res = x"12_bb" severity failure;

    wait;
  end process;
end behav;
