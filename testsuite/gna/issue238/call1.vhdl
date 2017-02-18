entity call1 is
end;

use work.pkg.all;

architecture behav of call1 is
  function func return rec is
    variable res : rec_4;
  begin
    return res;
  end func;
begin
  process
   variable v : rec_4 := func;
  begin
   wait;
  end process;
end behav;
