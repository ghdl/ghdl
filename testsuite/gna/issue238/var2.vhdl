entity var2 is
end;

use work.pkg.all;

architecture behav of var2 is
begin
  process
   variable v1, v2 : rec_4;
  begin
   v2 := v1;
   wait;
  end process;
end behav;
