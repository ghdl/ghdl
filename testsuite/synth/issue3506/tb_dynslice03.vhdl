library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb_dynslice03 is
end entity;

architecture behav of tb_dynslice03 is
  signal clk, we, w : std_logic := '0';
  signal d : std_logic_vector(15 downto 0) := x"0000";
  signal q : std_logic_vector(63 downto 0);
  procedure tick (signal c : out std_logic) is
  begin
    wait for 1 ns; c <= '1'; wait for 1 ns; c <= '0';
  end procedure;
begin
  dut : entity work.dynslice03 port map (clk, we, w, d, q);

  process
  begin
    we <= '1';
    w <= '1'; d <= x"2233"; tick (clk);
    w <= '0'; d <= x"bbcc"; tick (clk);
    we <= '0'; wait for 1 ns;
    assert q = x"0022_3300_00bb_cc00" severity failure;
    wait;
  end process;
end architecture;
