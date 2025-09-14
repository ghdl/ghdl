library ieee;
use ieee.std_logic_1164.all;

entity ent is
end entity;

architecture a of ent is
        type rec is record
                data : std_logic_vector;
                dummy : integer;
        end record;
 
        type arr is array(natural range <>) of rec;
 
        constant con : arr :=
        (
                ( "000", 0 ),
                ( "00000", 1 )
        );
begin
        process is
        begin
                for i in con'range loop
                        report to_string(i);
                end loop;
                wait;
        end process;
end architecture;
