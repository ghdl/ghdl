package repro3 is
  constant a1 : natural := 1;
  constant a2 : natural := 1;

  function f (arg : bit; length : string (1 to a1)) return bit;
end;

package body repro3 is
  function f (arg : bit; length : string (1 to a2)) return bit is
  begin
    return arg;
  end;
end;
