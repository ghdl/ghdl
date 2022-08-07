library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package closely_related_arrays is

  type sulv_vector is array (natural range <>) of std_ulogic_vector;
  type unsigned_vector is array (natural range <>) of u_unsigned;

  constant sv : sulv_vector(1 downto 0)(7 downto 0) := (1 => x"FF", 0 => x"00");
  constant uv : unsigned_vector(sv'range)(sv'element'range) := unsigned_vector(sv);

end package;
