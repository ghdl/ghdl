entity repro2 is
  
end repro2;

package repro2_pkg is
  procedure inc (a : inout integer);
  procedure inc (a : inout time);
end repro2_pkg;

package body repro2_pkg is
  impure function inc (a : integer) return integer is
  begin
    return a + 1;
  end inc;

  procedure inc (a : inout integer) is
  begin
    a := a + 1;
  end inc;
  
  procedure inc (a : inout time) is
  begin
    a := a + 1 ns;
  end inc;

  type t is (enum1, inc);
  
  impure function inc (a : time) return time is
  begin
    return a + 1 ns;
  end inc;

end repro2_pkg;

use work.repro2_pkg.all;
architecture behav of repro2 is
begin  -- behav
  process
    variable a : integer := 2;
  begin
    inc (a);
    assert a = 3 report "bad value of a";
    wait;
  end process;
end behav;
