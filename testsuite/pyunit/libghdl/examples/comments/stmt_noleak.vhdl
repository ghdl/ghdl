architecture arch of e is
  procedure pr is
    --  before 'begin', discarded
  begin
    n : null;
    --  before 'end', discarded
  end pr;
  --  for :q:
  signal q : bit;
  --  before 'begin', discarded
begin
  p1 : process
    --  before 'begin', attached to the process (no declarations) :p1:
  begin
    st1 : wait;
    --  before 'end', discarded
  end process;

  --  for :st2:
  st2 : s <= q;

  p2 : process
    variable v : bit;
    --  before 'begin', discarded
  begin
    --  before 'end', discarded
  end process;

  b : block
  begin
    --  before 'end', discarded
  end block;

  --  for :st3:
  st3 : s <= q;

  g : for i in 0 to 1 generate
    --  before 'end', discarded
  end generate;

  --  for :st4:
  st4 : s <= q;

  --  before 'end', discarded
end architecture;

--  for :st5:
architecture st5 of e is
begin
end architecture;
