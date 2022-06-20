package repro2 is
  function f (arg : bit; length : string) return bit;
end repro2;

package body repro2 is
  function f (arg : bit; length : bit_vector) return bit is
  begin
    return arg;
  end;
end repro2;
