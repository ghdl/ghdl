library ieee;
use ieee.std_logic_1164.all;

entity issue3 is
end issue3;

architecture beh of issue3 is
    type t_rec is
        record
            elem : std_logic_vector (3 downto 0);
        end record;
begin
    assert t_rec'(elem => 4b"0") = t_rec'(elem => 3b"0");
end architecture beh;

