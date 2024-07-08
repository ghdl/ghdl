entity nolatch1 is
  port (sel : bit;
        r : out bit);
end;

architecture arch of nolatch1 is
begin
  process (all)
    variable a : bit;
  begin
    r <= '1';
    if sel = '1' then
      a := '1';
      r <= '0';
    end if;
  end process;
end;

