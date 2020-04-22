entity tb_multiplexers_3 is
end tb_multiplexers_3;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_multiplexers_3 is
  signal di : std_logic_vector(7 downto 0);
  signal sel : std_logic_vector(7 downto 0);
  signal do : std_logic;
begin
  dut: entity work.multiplexers_3
    port map (di, sel, do);

  process
  begin
    di  <=     b"1001_0011";
    sel <= not b"0000_0000";
    wait for 1 ns;
    assert do = 'Z' severity failure;

    di  <=     b"1001_0011";
    sel <= not b"0000_0001";
    wait for 1 ns;
    assert do = '1' severity failure;

    di  <=     b"1001_0011";
    sel <= not b"0000_0001";
    wait for 1 ns;
    assert do = '1' severity failure;

    di  <=     b"1001_0011";
    sel <= not b"0001_0001";
    wait for 1 ns;
    assert do = '1' severity failure;

    di  <=     b"1001_0011";
    sel <= not b"0010_0000";
    wait for 1 ns;
    assert do = '0' severity failure;

    di  <=     b"1001_0011";
    sel <= not b"0110_1100";
    wait for 1 ns;
    assert do = '0' severity failure;

    di  <=     b"1001_0011";
    sel <= not b"0010_0001";
    wait for 1 ns;
    assert do = 'X' severity failure;

    wait;
  end process;
end behav;
