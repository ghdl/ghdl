package gen_pkg is
  generic (
    type item_t
  );
end package gen_pkg;

library ieee;
  use ieee.std_logic_1164.all;

package type_pkg is
  subtype r_item_t is std_ulogic_vector(15 downto 0);
  constant R_EMPTY : r_item_t := "XXXXXXXXXXXXXXXX";
end package type_pkg;

library ieee;
  use ieee.std_logic_1164.all;
use work.type_pkg.all;
package r_gen_pkg is
  new work.gen_pkg generic map(item_t => r_item_t);
