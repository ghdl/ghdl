library ieee;
use ieee.std_logic_1164.all;

entity issue4 is
end issue4;

architecture beh of issue4 is
    type t_rec is
        record
            elem : std_logic_vector (3 downto 0);
        end record;

    signal foo : std_logic_vector (4 downto 0);
begin
    assert t_rec'(elem => foo) = t_rec'(elem => foo);
end architecture beh;
