entity tb_test_addsub is
end tb_test_addsub;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture behav of tb_test_addsub is
  signal s_slv        : std_logic_vector(3 downto 0);
  signal s_nat        : natural range 0 to 15;
  signal s_add_slvslv : std_logic_vector(3 downto 0);
  signal s_add_slvnat : std_logic_vector(3 downto 0);
  signal s_add_natslv : std_logic_vector(3 downto 0);
  signal s_sub_slvslv : std_logic_vector(3 downto 0);
  signal s_sub_slvnat : std_logic_vector(3 downto 0);
  signal s_sub_natslv : std_logic_vector(3 downto 0);
begin
  dut: entity work.test_addsub
    port map (s_slv,
              s_nat,
              s_add_slvslv,
              s_add_slvnat,
              s_add_natslv,
              s_sub_slvslv,
              s_sub_slvnat,
              s_sub_natslv);
  process
  begin
    s_slv <= x"6";
    s_nat <= 6;
    wait for 1 ns;
    assert s_add_slvslv = x"C" severity failure;
    assert s_add_slvnat = x"C" severity failure;
    assert s_add_natslv = x"C" severity failure;
    assert s_sub_slvslv = x"0" severity failure;
    assert s_sub_slvnat = x"0" severity failure;
    assert s_sub_natslv = x"0" severity failure;

    s_slv <= x"6";
    s_nat <= 10;
    wait for 1 ns;
    assert s_add_slvslv = x"C" severity failure;
    assert s_add_slvnat = x"0" severity failure;
    assert s_add_natslv = x"0" severity failure;
    assert s_sub_slvslv = x"0" severity failure;
    assert s_sub_slvnat = x"C" severity failure;
    assert s_sub_natslv = x"4" severity failure;

    wait;
  end process;
end behav;
