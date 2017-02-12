entity var4 is
end;

use work.pkg.all;

architecture behav of var4 is
begin
  process
   variable v1 : rec_4;
   variable v2 : rec_4dyn;
  begin
   v2 := v1;
   wait;
  end process;
end behav;
