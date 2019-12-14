library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test is
    port(
        clk     : in std_logic;
        wr_addr : in std_logic_vector(0 downto 0);
        wr_data : in std_logic_vector(7 downto 0)
        );
end test;

architecture rtl of test is
    type ram_type is array (0 to 1) of std_logic_vector(7 downto 0);
    signal ram : ram_type := (others => (others => '0'));
begin
    process(clk)
        variable widx : integer range 0 to 1;
    begin
        if rising_edge(clk) then
            widx := to_integer(unsigned(wr_addr));
            ram(widx) <= wr_data;
        end if;
    end process;
end;
