library ieee;
use ieee.std_logic_1164.all;

entity tc3 is
    port (
        state   : in std_ulogic;
        o       : out std_ulogic_vector(3 downto 0)
        );
end entity tc3;

architecture behaviour of tc3 is
    signal misc_sel      : std_ulogic_vector(3 downto 0);
begin
    testcase_0: process(all)
    begin
        misc_sel <= "0000";

        if state = '0' then
          misc_sel <= "0111";
        else
          misc_sel(3) <= '1';
        end if;

        o <= misc_sel;
    end process;
end architecture behaviour;
