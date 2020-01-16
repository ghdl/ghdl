library ieee;
use ieee.std_logic_1164.all;

package mask_pkg is
  constant MASK: std_logic_vector(0 downto 0);
end package;

package body mask_pkg is
  constant MASK: std_logic_vector(0 downto 0) := "0";
end package body;

library ieee;
use ieee.std_logic_1164.all;

entity my_entity is
  port (
    data: out std_logic_vector(0 downto 0)
  );
end entity;

use work.mask_pkg.all;

architecture arch of my_entity is
begin
  data <= MASK;
end architecture;
