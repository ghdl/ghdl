entity e is
end entity;

architecture a of e is
begin
  process
    type LongInteger is range -2**47 to 2**47-1;
    variable v : LongInteger;
  begin
    v := 12345678901;
    report "v = " & LongInteger'image(v) severity note;
    -- report "v = " & to_string(v) severity note;                    -- works
    -- report "v = " & to_string(LongInteger'pos(v)) severity note;   -- works
    wait;
  end process;
end architecture;
