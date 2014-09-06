entity repro3 is

end repro3;

package repro3_pkg is
  procedure inc (a : inout integer);
  type prot is protected
    procedure get (a : integer);
  end protected prot;
end repro3_pkg;

package body repro3_pkg is
  procedure inc (a : inout integer) is
  begin
    a := a + 1;
  end inc;

  procedure inc (a : inout time) is
  begin
    a := a + 1 ns;
  end inc;

  type prot is protected body
    variable v : integer;

    function inc (a : integer) return integer is
    begin
      return a + 1;
    end inc;

    procedure get (a : integer) is
    begin
      v := a;
    end get;
  end protected body prot;

end repro3_pkg;

use work.repro3_pkg.all;
architecture behav of repro3 is
begin  -- behav
  process
    variable a : integer := 2;
  begin
    inc (a);
    assert a = 3 report "bad value of a";
    wait;
  end process;
end behav;
