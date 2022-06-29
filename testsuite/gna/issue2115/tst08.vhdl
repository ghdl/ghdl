entity tst08 is
end entity;

use std.textio.all;

architecture a of tst08 is
begin
  process
    variable l : line;
  begin
    write(l, false);
    assert l.all = "false" severity failure;
    deallocate (l);
    write(l, true);
    assert l.all = "true" severity failure;

    assert boolean'image(true) = "true" severity failure;
    assert boolean'image(false) = "false" severity failure;

    assert to_string(true) = "true" severity failure;
    assert to_string(false) = "false" severity failure;
    wait;
  end process;
end;
