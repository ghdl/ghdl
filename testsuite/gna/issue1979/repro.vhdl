package repro_pkg is
  function "<" (a, b : natural) return bit;
end repro_pkg;

package body repro_pkg is
  function "<" (a, b : natural) return bit is
  begin
    if boolean'(a < b) then
      return '1';
    else
      return '0';
    end if;
  end "<";
end repro_pkg;

use work.repro_pkg.all;
entity repro is
end;

architecture behav of repro is
  signal n1, n2 : natural;
begin
  --  No crash: this is a vhdl expression.
  --  assert n1 < n2;

  --  No crash: this is an 'or' property
  --  assert n1 < n2 or n2 < n1;

  --  Crash: this is a vhdl expression
  assert (n1 < n2) or (n2 < n1);
end;
