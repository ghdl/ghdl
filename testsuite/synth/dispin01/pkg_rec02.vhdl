library ieee;
use ieee.std_logic_1164.all;

package rec02_pkg is
  type myrec is record
     a : natural range 0 to 5;
     b : std_logic;
  end record;
end rec02_pkg;
