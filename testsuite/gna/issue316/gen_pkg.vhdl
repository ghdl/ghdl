package gen_pkg is
  generic (
    type T;
    function "-"(a : T)    return T-- is <>;
  );
end package;

use work.gen_pkg;

package p is new gen_pkg
  generic map (
    T => T_1000
  );
