entity call4 is
end;

use work.pkg.all;

architecture behav of call4 is
  procedure p (s : string) is
  begin
    report natural'image (s'left);
    report natural'image (s'right);
    assert s'left = 1;
    assert s'right = 4;
  end;
begin
  process
    variable v : rec_4dyn;
  begin
    p (v.s);
   wait;
  end process;
end behav;
