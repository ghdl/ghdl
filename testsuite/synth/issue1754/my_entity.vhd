library ieee;
use ieee.std_logic_1164.all;

entity my_entity is
end;

architecture behaviour of my_entity is
    type std_logic_vector_array_t is array (natural range <>) of std_logic_vector;
    type reg_t is record
        x : std_logic_vector_array_t(15 downto 0)(15 downto 0);
    end record;

begin

end;
