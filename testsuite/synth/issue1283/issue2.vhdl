library ieee;
use ieee.std_logic_1164.all;

entity issue2 is
end issue2;

architecture beh of issue2 is
    type t_rec is
        record
            elem : std_logic_vector (3 downto 0);
        end record;

    function fun (arg : std_logic_vector) return t_rec is
    begin
        return t_rec'(elem => arg);
    end function;
begin
    -- wrong length
    -- -a accepts
    -- -synth error + bug report
    assert fun ("000") = fun ("000");
end architecture beh;
