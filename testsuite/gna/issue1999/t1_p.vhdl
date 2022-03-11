-----
---  testcase for operator overload "/"
-----

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.unsigned;
use ieee.numeric_std.resize;

package t1_p is
  function resize(signal i : std_logic_vector; constant n : integer)
    return std_logic_vector;
  function "/" (signal a : std_logic_vector; signal b : std_logic_vector)
    return std_logic_vector;
end package t1_p;

package body t1_p is
  function resize(
    signal i : std_logic_vector; constant n : integer) return std_logic_vector is
  begin
    return std_logic_vector(ieee.numeric_std.resize(unsigned(i), n));
  end function;

  -- purpose: or signals with different sizes
  function "/" (   -- this is NOT a divider operand  !!!
    signal a : std_logic_vector;
    signal b : std_logic_vector)
    return std_logic_vector is
    variable s : integer;
  begin  -- 
    s := a'length;
    if b'length > a'length then
      s := b'length;
    end if;
    return resize(a, s) or resize(b, s);
  end "/";
end t1_p;

