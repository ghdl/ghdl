library ieee;
use ieee.std_logic_1164.all;

entity test is
end entity test;

architecture rtl of test is
    type reg_internal_type is record
        outstanding : integer;
    end record;
begin
    control1 : process(all)
        variable v_int : reg_internal_type;
    begin
    end process;
end;
