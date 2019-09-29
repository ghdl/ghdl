library ieee;
use ieee.std_logic_1164.all;

entity ent is
    port (
        i : in bit;
        o : out bit
    );
end;

architecture a of ent is
    signal test : std_logic_vector(7 downto 0);
    alias a : std_logic_vector(7 downto 0) is test;
begin
    process(i)
    begin
        if a = x"00" then
        end if;

        o <= i;
    end process;
end;
