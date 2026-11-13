entity repro1_leaf is
end repro1_leaf;

architecture rtl of repro1_leaf is
  signal r : bit_vector (3 downto 0) := "0000";
  signal v : bit := '0';
begin
  v <= '1' after 2 ns;
end rtl;

entity repro1 is
end repro1;

architecture tb of repro1 is
begin
  dut : entity work.repro1_leaf;

  process
    alias r is << signal dut.r : bit_vector (3 downto 0) >>;
    alias v is << signal dut.v : bit >>;
  begin
    r <= force "0101";
    wait until v = '1';
    assert now = 2 ns severity failure;
    assert r = "0101" severity failure;
    r <= release;
    wait for 1 ns;
    assert r = "0000" severity failure;
    report "done";
    wait;
  end process;
end tb;
