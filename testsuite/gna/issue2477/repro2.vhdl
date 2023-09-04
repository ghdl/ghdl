library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package top_pkg is
  type mm_element_t is record
    name: string;
    base: unsigned (31 downto 0);
    size: unsigned (31 downto 0);
  end record;
  type memmap_t is array (0 to 2) of mm_element_t;
  constant top: memmap_t :=
    (0 => ("null", 32x"0", 32x"0"),
     1 => ("reg1", 32x"0", 32x"1000"),
     2 => ("reg2", 32x"1000", 32x"480"));
end package;

entity repro2 is
end;

use work.top_pkg.all;

architecture arch of repro2 is
begin
  assert top (0).name = "null";
end arch;
