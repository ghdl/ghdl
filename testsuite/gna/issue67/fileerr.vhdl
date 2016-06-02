entity fileerr is
end fileerr;

use std.textio.all;

architecture behav of fileerr is
begin
  process
    file f : text;
  begin
    file_open (f, "bad-file-name");
    wait;
  end process;
end behav;
