library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb_mux01 is
end entity;

architecture behav of tb_mux01 is
  signal v : std_logic_vector(3 downto 0);
  signal o1, o2 : std_logic;
begin
  dut : entity work.mux01
    port map (s1 => v(0), s2 => v(1), x => v(2), y => v(3),
              o1 => o1, o2 => o2);

  process
    variable g, c : std_logic;
  begin
    for i in 0 to 15 loop
      v <= std_logic_vector(to_unsigned(i, 4));
      wait for 1 ns;
      if v(0) = '1' then g := v(2); else g := v(3); end if;
      if v(1) = '1' then c := g; else c := v(3); end if;
      assert o1 = g report "bad o1" severity failure;
      assert o2 = c report "bad o2" severity failure;
    end loop;
    wait;
  end process;
end architecture;
