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
end entity;

architecture mixed of top is
  signal u, v: foo;
  signal b : boolean;
begin
  b <= u = v;
end architecture;
