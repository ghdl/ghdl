library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package rec06_pkg is
  type myrec2 is record
    c : natural range 0 to 3;
    d : unsigned (3 downto 0);
  end record;

  type myrec is record
    a : myrec2;
    b : std_logic;
  end record;
end rec06_pkg;
