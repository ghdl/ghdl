entity repro2_leaf is
end repro2_leaf;

architecture rtl of repro2_leaf is
  signal v : bit := '1';
begin
end rtl;

entity repro2 is
end repro2;

architecture tb of repro2 is
begin
  dut : entity work.repro2_leaf;

  process
    procedure check is
      alias v is << signal .repro2.dut.v : bit >>;
    begin
      assert v = '1' severity failure;
    end check;
  begin
    wait for 1 ns;
    check;
    report "done";
    wait;
  end process;
end tb;
