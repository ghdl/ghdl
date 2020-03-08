library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity func01 is
  port (a : std_logic_vector (7 downto 0);
        b : out std_logic_vector (7 downto 0));
end func01;

architecture behav of func01 is
  function "+"(l, r : std_logic_vector) return std_logic_Vector is
  begin
    return std_logic_vector(unsigned(l) + unsigned(r));
  end "+";
begin
  b <= a + a;
end behav;

