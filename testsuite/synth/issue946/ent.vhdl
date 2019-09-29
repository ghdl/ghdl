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
    alias a : std_logic_vector(3 downto 0) is test(7 downto 4);
begin
    process(i)
    begin
        case a(1 downto 0) is
            when others =>
        end case;

        o <= i;
    end process;
end;
