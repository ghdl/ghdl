entity var3 is
end;

use work.pkg.all;

architecture behav of var3 is
begin
  process
   variable v1 : rec_4;
   variable v2 : rec_4bis;
  begin
   v2 := v1;
   wait;
  end process;
end behav;
