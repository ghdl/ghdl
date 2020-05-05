library ieee;
use ieee.std_logic_1164.all;

entity issue1 is
end issue1;

architecture beh of issue1 is
    type t_rec is
        record
            elem : std_logic_vector;
        end record;
begin
    assert t_rec'(elem => "000") = t_rec'(elem => "000");
    assert t_rec'(elem => "001") = t_rec'(elem => "000") severity note;
end architecture beh;
