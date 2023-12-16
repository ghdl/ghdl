----------------------------------------------------------------------
package generic_package is
  generic (
    type foo;
    constant nil, bar: foo;
    function to_string(a: foo) return string is <>
  );
  function consensus(a, b: foo) return foo;
end package;

package body generic_package is
  function consensus(a, b: foo) return foo is
    variable res: foo := nil;
  begin
    res := bar when a = bar and b = bar;
    return res;
  end function;
end package body;
----------------------------------------------------------------------
entity generic_consensus is
  generic (
    type foo;
    function consensus(a, b: foo) return foo is <>
  );
  port (a, b: in foo; res: out foo);
end entity;

architecture dataflow of generic_consensus is
begin
  res <= consensus(a, b);
end architecture;
----------------------------------------------------------------------
package base_variant is
  type foo is (A0, A1);
  constant nil: foo := A0;
  constant bar: foo := A1;
end package;
----------------------------------------------------------------------
use work.base_variant.all;
package inst_package is new work.generic_package
  generic map (foo => foo, nil => nil, bar => bar);
----------------------------------------------------------------------
use work.inst_package.all;
entity top is
  port (x, y, z: in foo; res: out foo);
end entity;

architecture mixed of top is
  signal u, v: foo;
begin
  u <= consensus(x, y);
  v <= consensus(y, z);
  e: entity work.generic_consensus(dataflow)
       generic map (foo => foo) port map (a => u, b => v, res => res);
end architecture;
----------------------------------------------------------------------
use work.inst_package.all;
use std.textio.all;

entity tb is end entity;

architecture mixed of tb is
  signal x, y, z, res: foo;
begin
  t: entity work.top(mixed)
    port map (x => x, y => y, z => z, res => res);

  x <= nil, bar after 3 ns;
  y <= nil, bar after 5 ns;
  z <= nil, bar after 4 ns, nil after 10 ns;

  log: postponed process (all) is
    variable lin: line;
  begin
    write(lin, to_string(now)
               & ", x: " & to_string(x)
               & ", y: " & to_string(y)
               & ", z: " & to_string(z)
               & ", res: " & to_string(res));
    writeline(output, lin);
  end process;
end architecture;
----------------------------------------------------------------------

