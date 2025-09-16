library ieee;
    use ieee.std_logic_1164.all;

entity TOP is
    port (
        ADDRESS : in    std_ulogic_vector(15 downto 0);
        OUTPUT : out   std_logic_vector(1 downto 0)
    );
end entity TOP;

architecture behave of TOP is

    subtype BASE_ADDRESS_RANGE is natural range 17 downto 2;

    constant BASE_A : std_ulogic_vector(31 downto 0) := X"FFFF_02--";
    constant BASE_B : std_ulogic_vector(31 downto 0) := X"AAAA_1" & B"00--_----_----";

begin

    select_proc : process (all) is
    begin

        OUTPUT <= "00";

        case ? ADDRESS is

            when BASE_A(BASE_ADDRESS_RANGE) =>

                OUTPUT <= "10";

            when BASE_B(BASE_ADDRESS_RANGE) =>

                OUTPUT <= "01";

            when others =>

                null;

        end case?;

    end process select_proc;

end architecture behave;
