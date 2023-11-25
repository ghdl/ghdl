library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;



library work;
use work.all;


entity tb2 is
begin
end entity tb2;

architecture behv of tb2 is
  signal clk_tb	:std_logic;
begin
  th : entity work.test_harness;

  b : block
    alias clk is <<signal.tb2.th.clk:std_logic>>;
  begin
      clk_tb <= clk;
  end block;
end architecture behv;
