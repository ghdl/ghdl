--  wait until on an alias of an external name (the sensitivity is
--  registered on the signal denoted by the external name).
entity repro3_leaf is
end repro3_leaf;

architecture rtl of repro3_leaf is
  signal v : bit := '0';
begin
  v <= '1' after 2 ns;
end rtl;

entity repro3 is
end repro3;

architecture tb of repro3 is
begin
  dut : entity work.repro3_leaf;

  process
    alias v is << signal dut.v : bit >>;
  begin
    wait until v = '1';
    assert now = 2 ns severity failure;
    report "repro3 ok";
    wait;
  end process;
end tb;
