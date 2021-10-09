entity tb_test2 is
end;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_test2 is
  signal crnum_in : std_ulogic_vector(2 downto 0) := "000";
  signal cr_in    : std_ulogic_vector(31 downto 0);
  signal crf_out  : std_ulogic_vector(3 downto 0);
begin
  dut: entity work.test2
    port map (crnum_in, cr_in, crf_out);

  process
  begin
    cr_in <= x"76543210";

    crnum_in <= "000";
    wait for 1 ns;
    assert crf_out = x"0" severity failure;

    crnum_in <= "001";
    wait for 1 ns;
    assert crf_out = x"1" severity failure;

    crnum_in <= "100";
    wait for 1 ns;
    assert crf_out = x"4" severity failure;

    crnum_in <= "111";
    wait for 1 ns;
    assert crf_out = x"7" severity failure;

    wait;
  end process;
end behav;
