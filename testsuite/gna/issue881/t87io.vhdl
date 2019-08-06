entity t87io is
end;

use std.textio.all;

architecture behav of t87io is
  constant t1 : time := 1 ns;
  constant t2 : natural := time'pos (t1);
begin
  assert t1 = 1000 ps;
  process
    variable v : natural;
  begin
    --  Time resolution must be ps
    v := time'pos(ps);
    assert v = 1 severity failure;
    wait;
  end process;
end behav;
