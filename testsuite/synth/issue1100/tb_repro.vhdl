entity tb_repro is
end tb_repro;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_repro is
  signal val   : std_logic_vector (63 downto 0);
  signal iperm : std_logic_vector (3*8 - 1 downto 0) := (others => '0');
  signal en    : std_ulogic;
  signal res   : std_logic_vector (63 downto 0);
begin
  dut: entity work.repro
    port map (val, iperm, en, res);

  process
  begin
    val <= x"01_23_45_67_89_ab_cd_ef";
    en <= '1';
    iperm <= o"76543210";
    wait for 1 ns;
    assert res = x"01_23_45_67_89_ab_cd_ef"severity failure;

    iperm <= o"01234567";
    wait for 1 ns;
    assert res = x"ef_cd_ab_89_67_45_23_01"
      report to_hstring(res) severity failure;
wait;
  end process;
end behav;
