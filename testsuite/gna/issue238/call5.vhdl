entity call5 is
end;

use work.pkg.all;

architecture behav of call5 is
  procedure p2 (s : string) is
  begin
    report natural'image (s'left);
    report natural'image (s'right);
    assert s'left = 1;
    assert s'right = 4;
  end;

  procedure p1 (r : rec) is
  begin
    p2 (r.s);
  end p1;
  
begin
  process
    variable v : rec_4dyn;
  begin
    p1 (v);
   wait;
  end process;
end behav;
