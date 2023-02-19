
library ieee;
use ieee.std_logic_1164.all;

library osvvm;
library work;

package b is
    generic (
        G_b: integer
    );

    package a_inst is new work.a generic map (G_b);
        use a_inst.all;

    -- OSVVM string helper function
    function ToString(
        i_my_type   : a_inst.my_type_t
    ) return string;

    -- OSVVM compare helper function
    function Compare(
        i_my_type_a : a_inst.my_type_t;
        i_my_type_b : a_inst.my_type_t
    ) return boolean;

    -- scoreboard specificiation
    package scoreboard is new osvvm.ScoreBoardGenericPkg
    generic map (
        ExpectedType        => a_inst.my_type_t,
        ActualType          => a_inst.my_type_t,
        match               => Compare,
        expected_to_string  => ToString,
        actual_to_string    => ToString
    );


end package b;

package body b is

    function ToString(
        i_my_type   : a_inst.my_type_t
    ) return string is
        constant c_str  : string    := "Num el: " & integer'image(i_my_type.a'length);
    begin
        return c_str;
    end function ToString;

    -- OSVVM compare helper function
    function Compare(
        i_my_type_a : a_inst.my_type_t;
        i_my_type_b : a_inst.my_type_t
    ) return boolean is
        variable v_rtn : boolean;
    begin
        v_rtn := true;

        for idx in i_my_type_a.a'range loop
            if i_my_type_a.a(idx) /= i_my_type_b.a(idx) then
                v_rtn := false;
            end if;
        end loop;

        return v_rtn;
    end function Compare;

end package body b;