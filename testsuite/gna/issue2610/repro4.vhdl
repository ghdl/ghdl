--  An alias of an external name declared in a subprogram, and an external
--  name used directly in a concurrent statement.
entity repro4_leaf is
end repro4_leaf;

architecture rtl of repro4_leaf is
  signal v : bit := '1';
begin
end rtl;

entity repro4 is
end repro4;

architecture tb of repro4 is
  signal m : bit;
begin
  dut : entity work.repro4_leaf;

  m <= << signal dut.v : bit >>;

  process
    procedure check is
      alias v is << signal .repro4.dut.v : bit >>;
    begin
      assert v = '1' severity failure;
    end check;
  begin
    wait for 1 ns;
    check;
    assert m = '1' severity failure;
    report "repro4 ok";
    wait;
  end process;
end tb;
