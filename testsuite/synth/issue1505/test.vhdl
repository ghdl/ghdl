library ieee;
use ieee.std_logic_1164.all;

entity test is
    port (
        x: in integer range 0 to 9;
        y: in integer range 0 to 10;
        o: out std_ulogic
    );
end entity;

architecture arch of test is
    -- Test 2D read-only array with non-power-of-2 dimensions
    type arr is array (0 to 10, 0 to 9) of std_ulogic;
    constant rom: arr := (
        ('1', '0', '0', '0', '0', '0', '0', '0', '0', '0'),
        ('0', '1', '0', '0', '0', '0', '0', '0', '0', '0'),
        ('0', '0', '1', '0', '0', '0', '0', '0', '0', '0'),
        ('0', '0', '0', '1', '0', '0', '0', '0', '0', '0'),
        ('0', '0', '0', '0', '1', '0', '0', '0', '0', '0'),
        ('0', '0', '0', '0', '0', '1', '0', '0', '0', '0'),
        ('0', '0', '0', '0', '0', '0', '1', '0', '0', '0'),
        ('0', '0', '0', '0', '0', '0', '0', '1', '0', '0'),
        ('0', '0', '0', '0', '0', '0', '0', '0', '1', '0'),
        ('0', '0', '0', '0', '0', '0', '0', '0', '0', '1'),
        ('0', '0', '0', '0', '0', '0', '0', '0', '0', '0')
    );
begin
    o <= rom(y, x);
end architecture;
