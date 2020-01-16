-- Machine generated from ram.img.
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package bootrom is
  type rom_t is array (0 to 127) of std_logic_vector(31 downto 0);
  constant rom : rom_t := (
    x"00000110",
    x"00001ffc",
    x"00000110",
-- more stuff, doesn't matter as long as it fits...
    x"23811fac",
    x"00afffac",
    others => x"00000000" );

end package;

package body bootrom is
end package body;
