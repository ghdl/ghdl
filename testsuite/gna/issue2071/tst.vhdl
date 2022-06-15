library ieee;
use ieee.std_logic_1164.all;

package TST_PKG is
    type Indices_t is array (natural range <>) of std_logic_vector;

    type Bus_t is record
        Indices : Indices_t;
    end record;

    function Init(
        TST_PKG_bus : Bus_t
    ) return Bus_t;

end package;

package body TST_PKG is
    function Init(
        TST_PKG_bus : Bus_t
    )
    return Bus_t is
        variable result : Bus_t(
            Indices(TST_PKG_bus.Indices'range)(TST_PKG_bus.Indices'element'range)
        );
    begin
        result.Indices := (others => (others => '0'));
        return result;
    end function Init;
end package body;

library ieee;
use ieee.std_logic_1164.all;
use work.tst_pkg.all;

entity tst is
end;

architecture arch of tst is
  constant c1 : bus_t := (indices => (1 to 4 => "01"));
  constant c2 : bus_t := init (c1);
begin
end;
