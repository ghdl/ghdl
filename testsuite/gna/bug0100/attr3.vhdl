entity attr3 is
end attr3;

use std.textio.all;

architecture behav of attr3 is
  signal s : bit;
begin
  process
    variable l : line;
  begin
    wait until s = '1';
    'event;

    if s = '0' then
      write(l, s);
    else
      write(l, string'("hello"));
      write(l, s);
    end if;
  end process;
end behav;
