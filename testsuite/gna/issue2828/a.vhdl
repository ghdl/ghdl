entity a is
  port (o : out bit);
end entity;

architecture arch of a is
  signal cnt : natural;
begin
  cnt <= cnt + 1 after 10 ns;

  process (cnt)
    variable v : bit;
  begin
    v := '1' when cnt = 5 else '1' when cnt = 7 else '0';
    o <= v;
  end process;
end;
