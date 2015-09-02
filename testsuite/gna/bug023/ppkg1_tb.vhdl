entity ppkg1_tb is
end ppkg1_tb;

use work.ppkg1.all;
architecture behav of ppkg1_tb is
begin
  process
  begin
    rep1;
    rep2;
    rep3;
    wait;
  end process;
end behav;
