entity call2 is
end;

use work.pkg.all;

architecture behav of call2 is
  function func return rec is
    variable res : rec_4;
  begin
    return res;
  end func;
begin
  process
   constant v : rec := func;
  begin
   wait;
  end process;
end behav;
