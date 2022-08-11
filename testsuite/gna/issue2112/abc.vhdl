library ieee;
use ieee.std_logic_1164.all;

package abc is
    type Parameters_t is record
        BW    : natural;
        PAIRS : natural;
    end record;

    type Indices_t is array (natural range <>) of std_logic_vector;

    type Bus_t is record
        Indices : Indices_t;
    end record;

    function Test(
        abc_bus : Bus_t
    ) return Bus_t;

    function Test(
        abc_bus : Bus_t;
        indices : Indices_t
    ) return Bus_t;
end package;

package body abc is
    function Test(
        abc_bus : Bus_t;
        indices : Indices_t
    ) return Bus_t is
        variable result : Bus_t(
            Indices(abc_bus.Indices'range)(abc_bus.Indices'element'range)
        ) := Test(abc_bus);
    begin
        return result;
    end function;

    function Test(
        abc_bus : Bus_t
    ) return Bus_t is
        variable result : Bus_t(
            Indices(abc_bus.Indices'range)(abc_bus.Indices'element'range)
        ) := abc_bus;
    begin
        return result;
    end function;
end package body;


