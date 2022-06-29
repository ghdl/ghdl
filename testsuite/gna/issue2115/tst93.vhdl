entity tst93 is
end entity;

use std.textio.all;

architecture a of tst93 is
begin
  process
    variable l : line;
  begin
    write(l, false);
    assert l.all = "FALSE" severity failure;
    deallocate (l);
    write(l, true);
    assert l.all = "TRUE" severity failure;

    assert boolean'image(true) = "true" severity failure;
    assert boolean'image(false) = "false" severity failure;
    wait;
  end process;
end;
