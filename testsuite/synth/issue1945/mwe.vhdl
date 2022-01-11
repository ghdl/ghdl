library ieee;
use ieee.STD_LOGIC_1164.all;

entity MWE is
    port(
        i : in  std_logic_vector(7 downto 0);
        o : out std_logic
    );
end entity;

architecture rtl of MWE is
    alias i_alias : std_logic_vector(7 downto 0) is i;
    -- this crashes:
    alias lower   : std_logic_vector(3 downto 0) is i_alias(3 downto 0);
    -- this works fine:
    -- alias lower : std_logic_vector(3 downto 0) is i(3 downto 0);
begin
    -- this works fine too:
    -- o <= '1' when lower(3 downto 0) = "0000" else '0';

    process(all)
    begin
        if lower = "0000" then
            o <= '1';
        else
            o <= '0';
        end if;
    end process;
end architecture;
