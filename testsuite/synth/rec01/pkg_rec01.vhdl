library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package rec01_pkg is
  type myrec is record
     a : unsigned (3 downto 0);
     b : std_logic;
  end record;
end rec01_pkg;
