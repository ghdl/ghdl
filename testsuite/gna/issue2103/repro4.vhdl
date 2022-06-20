package repro4 is
  constant a1 : natural := 1;
  constant a2 : natural := 1;

  alias b1 : natural is a1;
  alias b2 : natural is a2;

  function f (arg : bit; length : string (1 to b1)) return bit;
end;

package body repro4 is
  function f (arg : bit; length : string (1 to b2)) return bit is
  begin
    return arg;
  end;
end;
