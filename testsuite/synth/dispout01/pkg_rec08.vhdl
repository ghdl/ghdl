library ieee;
use ieee.std_logic_1164.all;

package rec08_pkg is
  type myrec is record
     a : bit_vector (0 downto 0);
     b : std_logic;
  end record;
end rec08_pkg;
