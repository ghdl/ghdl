entity call3 is
end;

use work.pkg.all;

architecture behav of call3 is
  procedure p (a : rec) is
  begin
    report natural'image (a.s'left);
    report natural'image (a.s'right);
    assert a.s'left = 1;
    assert a.s'right = 4;
  end;
begin
  process
    variable v : rec_4dyn;
  begin
    p (v);
   wait;
  end process;
end behav;
