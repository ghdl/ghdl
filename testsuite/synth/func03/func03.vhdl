library ieee;
use ieee.std_logic_1164.all;

entity func03 is
  port (a1, a2, a3 : std_logic;
        b : out std_logic_vector (3 downto 0));
end;

architecture behav of func03 is
  function f(v : std_logic_vector) return std_logic_Vector is
    variable res : std_logic_vector(v'range);
  begin
    res := v;
    res (res'left) := '0';
    res (res'right) := '1';
    return res;
  end f;
begin
  assert f(v(0) => '1', v(1) => '1', v(2) => '0', v(3) => '0') = "0101";
  b <= a1&a2&a3&'0';
end behav;

