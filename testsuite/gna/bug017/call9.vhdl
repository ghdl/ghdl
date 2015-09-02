entity call9 is
end;

use std.textio.all;
architecture behav of call9 is
  procedure check_acc (l1, l2 : inout line) is
  begin
    assert l1 = null;
    assert l2 = null;

    l1 := new string'("Hello world");
    assert l1 /= null;
    assert l2 = null report "incorrect aliasing";

    l2 := new string'("second");
    assert l2 /= null;
    assert l2 /= l1 report "incorrect aliasing";
  end check_acc;
begin
  process
    variable l : line;
  begin
    check_acc (l, l);
    report "SUCCESS" severity note;
    wait;
  end process;
 
end behav;
