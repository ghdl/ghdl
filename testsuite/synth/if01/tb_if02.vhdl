entity tb_if02 is
end tb_if02;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_if02 is
  signal i, o: std_logic_vector(7 downto 0);
  signal s : std_logic;
begin
  dut: entity work.if02
    port map (i, s, o);

  process
  begin
    i <= b"01011010";
    s <= '0';
    wait for 1 ns;
    assert o = x"2d" severity failure;

    i <= b"01011010";
    s <= '1';
    wait for 1 ns;
    assert o = x"b4" severity failure;

    wait;
  end process;
end behav;
