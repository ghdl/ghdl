entity repro1 is
end entity;

architecture a of repro1 is
begin
  process
    type LongInteger is range -2**47 to 2**47-1;
    variable v : LongInteger;
    variable s : string (1 to 4);
  begin
    v := 12345678901;
    report "v = " & LongInteger'image(v) severity note;
    s := "1245";
    assert LongInteger'Value (s) = 1245 severity failure;
    wait;
  end process;
end architecture;
