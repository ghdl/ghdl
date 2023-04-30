package repro6_pkg is
  type bit_vec_vec is array (natural range <>) of bit_vector;

end repro6_pkg;

use work.repro6_pkg.all;

entity repro6_sub is
  port (i : bit_vec_vec(1 downto 0);
        o : out bit_vec_vec(1 downto 0));
end;

architecture behav of repro6_sub is
begin
  process (i)
  begin
    for j in i'range loop
      o(j) <= not i(j);
    end loop;
  end process;
end behav;

use work.repro6_pkg.all;

entity repro6 is
end;

architecture behav of repro6 is
  signal s1i, s1o, s2i, s2o : bit_vec_vec(1 downto 0)(3 downto 0);
begin
  inst1 : entity work.repro6_sub
    port map (i => s1i, o => s1o);
  inst2 : entity work.repro6_sub
    port map (i => s2i, o => s2o);

  process
  begin
    s1i (0) <= x"3";
    s1i (1) <= x"c";
    s2i (0) <= x"1";
    s2i (1) <= x"d";
    wait for 1 ns;
    assert s1o(0) = x"c" and s1o(1) = x"3" severity failure;
    assert s2o(0) = x"e" and s2o(1) = x"2" severity failure;

    s1i (0) <= x"4";
    s1i (1) <= x"a";
    s2i (0) <= x"5";
    s2i (1) <= x"6";
    wait for 1 ns;
    assert s1o(0) = x"b" and s1o(1) = x"5" severity failure;
    assert s2o(0) = x"a" and s2o(1) = x"9" severity failure;

    wait;
  end process;
end;
