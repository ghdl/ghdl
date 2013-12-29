library ieee;
use ieee.std_logic_1164.all;

entity bug2 is
end;

architecture this of bug2 is
  function f return integer is
    constant cc1: std_logic_vector := "1100";
    constant cc2: std_logic_vector := cc1;
    variable r: std_logic_vector(3 downto 0);
  begin
    assert false report "case2-a: "&integer'image(cc1'length) severity note;
    assert false report "case2-b: "&integer'image(cc2'length) severity note;--This reports "0". Correct one would be "4"!
    return 0;
  end;
  constant c1: std_logic_vector := "1010";
  constant c2: std_logic_vector := c1;
  signal i: integer;
begin
  process
  begin
    assert false report "case1-a: "&integer'image(c1'length) severity note;
    assert false report "case1-b: "&integer'image(c2'length) severity note;
    i <= f;
    wait;
  end process;
end;
