package repro4 is
  constant v : bit_vector := x"01";

  function f1 (a : natural := v'length) return natural;
  function f2 (a : natural := v'left) return natural;
  function f3 (a : natural := v'right) return natural;
  function f4 (a : natural := v'high) return natural;
  function f5 (a : natural := v'low) return natural;
  function f6 (a : boolean := v'ascending) return natural;
end;

package body repro4 is
  function f1 (a : natural := v'length) return natural is
  begin
    return 1;
  end;

  function f2 (a : natural := v'left) return natural is
  begin
    return 1;
  end;

  function f3 (a : natural := v'right) return natural is
  begin
    return 1;
  end;

  function f4 (a : natural := v'high) return natural is
  begin
    return 1;
  end;

  function f5 (a : natural := v'low) return natural is
  begin
    return 1;
  end;

  function f6 (a : boolean := v'ascending) return natural is
  begin
    return 1;
  end;
end;
