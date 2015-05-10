entity bug is
end;

use work.pkg.all;

architecture behav of bug is
begin
  p: process
    variable rec : rec_t;
  begin
    wait;
  end process;
end behav;
