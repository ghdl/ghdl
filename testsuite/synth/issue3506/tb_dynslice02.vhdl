library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb_dynslice02 is
end entity;

architecture behav of tb_dynslice02 is
  signal s, w : std_logic := '0';
  signal q : std_logic_vector(7 downto 0);
begin
  dut : entity work.dynslice02 port map (s, w, q);

  process
  begin
    s <= '0'; w <= '0'; wait for 1 ns;
    assert q = x"cd" severity failure;
    s <= '0'; w <= '1'; wait for 1 ns;
    assert q = x"45" severity failure;
    s <= '1'; w <= '0'; wait for 1 ns;
    assert q = x"32" severity failure;
    s <= '1'; w <= '1'; wait for 1 ns;
    assert q = x"ba" severity failure;
    wait;
  end process;
end architecture;
