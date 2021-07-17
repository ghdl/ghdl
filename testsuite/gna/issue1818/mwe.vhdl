library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ghdl_mwe is
port
(
    o_ArrayElementSize : out Natural
);
end entity ghdl_mwe;

architecture RTL of ghdl_mwe is

type t_TestArray is array (0 to 3) of std_logic_vector(8-1 downto 0);
constant TestArray : t_TestArray := (x"00", x"01", x"02", x"03");
    
begin

    o_ArrayElementSize <= TestArray'Element'Length;

end architecture RTL;

