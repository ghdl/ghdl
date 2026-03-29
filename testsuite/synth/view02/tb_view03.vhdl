library ieee;
use ieee.std_logic_1164.all;
use work.view03_pkg.all;

entity tb_view03 is
end;

architecture rtl of tb_view03 is
  signal s : bidir_t;
  signal clk, rst : std_logic;
begin
  dut : entity work.view03
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

    --  Reversed!
    s.rx.valid <= '0';
    s.tx.ready <= '1';
    pulse;

    rst <= '0';

    --  Present cycle
    assert s.rx.ready = '1' severity failure;
    assert s.tx.valid = '0' severity failure;
    s.rx.valid <= '1';
    s.rx.data <= x"f0";
    pulse;

    --  Result cycle.
    assert s.tx.valid = '1' severity failure;
    assert s.tx.data = x"0f" severity failure;
    s.rx.valid <= '0';
    pulse;

    wait;
  end process;
end architecture rtl;
