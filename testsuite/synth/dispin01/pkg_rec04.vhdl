library ieee;
use ieee.std_logic_1164.all;

package rec04_pkg is
  type myrec is record
     a : std_logic_vector (3 downto 0);
     b : std_logic;
  end record;
end rec04_pkg;
