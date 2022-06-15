library ieee;
use ieee.std_logic_1164.all;

entity testcase is
    port (
        clk : in std_logic;
        i   : in std_ulogic_vector(63 downto 0);
        o   : out std_ulogic_vector(63 downto 0)
        );
end entity testcase;

architecture behaviour of testcase is
    signal edge : std_ulogic_vector(63 downto 0) := (others => '1');
begin
    testcase_0: process(clk)
    begin
        if rising_edge(clk) then
            edge <= i;
            o <= edge;
        end if;
    end process;
end behaviour;
