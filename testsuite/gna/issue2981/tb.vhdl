library ieee;
use ieee.std_logic_1164.all;

entity function_range is
    generic(DATA_WIDTH  : natural := 64);

end entity function_range;

architecture tb of function_range is

    type UNCONSTRAINED_RANGE_RECORD is record
        data : std_logic_vector;
    end record UNCONSTRAINED_RANGE_RECORD;

    procedure procedure_issue_with_range (signal b : inout UNCONSTRAINED_RANGE_RECORD(data(DATA_WIDTH - 1 downto 0))) is
    begin

    end procedure procedure_issue_with_range;

    function funtion_issue_with_range (signal b : UNCONSTRAINED_RANGE_RECORD(data(DATA_WIDTH - 1 downto 0))) return boolean is
    begin

    end function funtion_issue_with_range;

begin

end architecture tb;
