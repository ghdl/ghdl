entity mytest is
end mytest;

library mylib;
use mylib.mypkg.all;

architecture behav of mytest is
begin
  process
  begin
    report msg severity note;
    wait;
  end process;
end behav;
