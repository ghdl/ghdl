library ieee;
use ieee.std_logic_1164.all;

entity blk1 is
end;

architecture sim of blk1 is
  signal clk : std_logic := '1';
  function m1 return integer is
  begin
    return -1;
  end;
begin
  b1: block
    generic (v : natural);
    generic map (v => m1);
  begin
    clk <= '1';
  end block;
end architecture;
