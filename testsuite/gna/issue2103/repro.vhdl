package repro is
  function f (arg : bit; length : natural) return bit;
end repro;

package body repro is
  function f (arg : bit; length : positive) return bit is
  begin
    return arg;
  end;
end repro;
