library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity testcase is
    port (
        misc_sel : in std_ulogic_vector(3 downto 0);
        result   : out std_ulogic_vector(63 downto 0)
        );
end entity testcase;

architecture behaviour of testcase is
begin
    testcase_0: process(all)
        variable misc : std_ulogic_vector(63 downto 0);
    begin
        case misc_sel is
            when "1101" =>
                misc := x"FFFFFFFF00000000";
                --misc := x"FFFFFFFF80000000";
            when others =>
                misc := x"0000000000000000";
        end case;
        result <= misc;
    end process;
end architecture behaviour;
