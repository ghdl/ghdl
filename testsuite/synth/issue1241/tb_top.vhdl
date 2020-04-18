entity tb_top is
end tb_top;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture behav of tb_top is
  signal sel : unsigned(1 downto 0);
  signal data : std_logic_vector(3 downto 0);
  signal q : std_logic;
begin
  dut: entity work.top
    port map (sel, data, q);

  process
  begin
    data <= "1001";
    sel <= "10";
    wait for 1 ns;
    assert q = '0' severity failure;

    sel <= "11";
    wait for 1 ns;
    assert q = '1' severity failure;

    sel <= "00";
    wait for 1 ns;
    assert q = '1' severity failure;

    sel <= "01";
    wait for 1 ns;
    assert q = '0' severity failure;

    wait;
  end process;
end behav;
