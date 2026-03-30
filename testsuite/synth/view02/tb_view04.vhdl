library ieee;
use ieee.std_logic_1164.all;
use work.view04_pkg.all;

entity tb_view04 is
end;

architecture rtl of tb_view04 is
  signal s : bus_t;
  signal clk, rst : std_logic;
begin
  dut : entity work.view04
    port map (clk => clk, rst => rst, b => s);

  process
    procedure pulse is
    begin
      clk <= '0';
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
    end pulse;
  begin
    rst <= '1';

    s.tx(1).valid <= '0';
    s.tx(2).valid <= '0';
    s.rx.ready <= '1';
    pulse;

    rst <= '0';

    --  Present cycle
    assert s.tx(1).ready = '1' severity failure;
    assert s.tx(2).ready = '1' severity failure;
    assert s.rx.valid = '0' severity failure;
    s.tx(1).valid <= '1';
    s.tx(1).data <= x"f0";
    s.tx(2).valid <= '1';
    s.tx(2).data <= x"a5";
    pulse;

    --  Compute cycle
    assert s.tx(1).ready = '0' severity failure;
    assert s.tx(2).ready = '0' severity failure;
    pulse;

    --  Result cycle.
    assert s.rx.valid = '1' severity failure;
    assert s.rx.data = x"55" severity failure;
    s.tx(1).valid <= '0';
    s.tx(2).valid <= '0';
    pulse;

    report "OK!" severity note;
    wait;
  end process;
end architecture rtl;
