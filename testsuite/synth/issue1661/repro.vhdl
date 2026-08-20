library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity repro is
    port (
        o : out std_logic_vector(7 downto 0)
    );
end entity;

architecture rtl of repro is
    -- integer'image is IIR_PREDEFINED_INTEGER_TO_STRING.
    constant C_STR : string := integer'image(42);
    constant C_LEN : natural := C_STR'length;
begin
    o <= std_logic_vector(to_unsigned(C_LEN, 8));
end architecture;
