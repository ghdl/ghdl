library ieee;
use ieee.std_logic_1164.all;

entity ExampleEntity is
    port (
        CLK : in std_logic;
        RST : in std_logic;
        DATA_OUT : out std_logic
    );
    attribute MY_ENTITY_ATTRIBUTE : string;
    attribute MY_ENTITY_ATTRIBUTE of ExampleEntity : entity is "User-defined metadata";
end entity ExampleEntity;

architecture Behavioral of ExampleEntity is
    signal DATA_REG : std_logic := '0';
begin
    process(CLK, RST)
    begin
        if RST = '1' then
            DATA_REG <= '0';
        elsif rising_edge(CLK) then
            DATA_REG <= not DATA_REG;
        end if;
    end process;

    DATA_OUT <= DATA_REG;
end architecture Behavioral;

