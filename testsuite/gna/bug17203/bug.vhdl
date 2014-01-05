use std.textio.all;

package read_string is
  function read_string_time (s : string) return time;
end read_string;

package body read_string is
  function read_string_time (s : string) return time is
    variable l       : line := new string'(s);
    variable t       : time;
    variable read_ok : boolean;
  begin
    read(l, t, read_ok);
    if not read_ok then
      report "read time failed" severity failure;
    end if;
    return t;
  end function;
end package body read_string;

use work.read_string.all;

entity test_time is
  generic (test_t : time := read_string_time("123 ps"));
end test_time;

architecture test of test_time is
begin
  process
    variable t : time;
  begin
    t := read_string_time("321 ps");
    report "t=" & time'image(t) severity warning;
    wait;
  end process;
end test;

