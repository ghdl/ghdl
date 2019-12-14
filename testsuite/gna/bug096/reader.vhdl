entity reader is
end reader;

use std.textio.all;

architecture behav of reader is
begin
  process
    file f : text is in "input.txt";
    variable l : line;
  begin
    for i in 1 to 5 loop
       readline (f, l);
    end loop;
    wait;
  end process;
end behav;
