package repro4 is
  alias n1 is natural;
  alias n2 is natural;

  function f (arg : bit; length : n1) return bit;
end;

package body repro4 is
  function f (arg : bit; length : n2) return bit is
  begin
    return arg;
  end;
end;
