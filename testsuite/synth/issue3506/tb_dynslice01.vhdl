library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb_dynslice01 is
end entity;

architecture behav of tb_dynslice01 is
  signal clk, we, s, w : std_logic := '0';
  signal d : std_logic_vector(7 downto 0) := x"00";
  signal q : std_logic_vector(63 downto 0);
  procedure tick (signal c : out std_logic) is
  begin
    wait for 1 ns; c <= '1'; wait for 1 ns; c <= '0';
  end procedure;
begin
  dut : entity work.dynslice01 port map (clk, we, s, w, d, q);

  process
  begin
    we <= '1';
    s <= '0'; w <= '1'; d <= x"33"; tick (clk);
    s <= '0'; w <= '0'; d <= x"cc"; tick (clk);
    s <= '1'; w <= '1'; d <= x"a5"; tick (clk);
    we <= '0';
    s <= '0'; wait for 1 ns;
    assert q = x"0000_3300_0000_cc00" severity failure;
    s <= '1'; wait for 1 ns;
    assert q = x"0000_a500_0000_0000" severity failure;
    wait;
  end process;
end architecture;
