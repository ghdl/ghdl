entity ppkg_tb is
end ppkg_tb;

use work.ppkg.all;
architecture behav of ppkg_tb is
begin
  process
  begin
    rep1;
    rep2;
    rep3;
    wait;
  end process;
end behav;
