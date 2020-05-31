entity tb_rotate_testcase is
end tb_rotate_testcase;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_Std.all;

architecture behav of tb_rotate_testcase is
  signal in_vec:  UNSIGNED(31 downto 0);
  signal out_vecl: UNSIGNED(31 downto 0);
  signal out_vecr:  UNSIGNED(31 downto 0);
begin
  dut: entity work.rotate_testcase
    port map (in_vec, out_Vecl, out_vecr);

  process
  begin
    in_vec <= x"1234_abcd";
    wait for 1 ns;
--    report to_hstring(out_vecr);
    assert out_vecl = x"2469579a" severity failure;
    assert out_vecr = x"891a55e6" severity failure;
    wait;
  end process;
end behav;
