library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity issue is
    port (clock : in  std_logic;
            bar : out signed(8-1 downto 0));
end issue;

architecture rtl of issue is
begin

    process (clock)
        variable foo : signed(8-1 downto 0);
    begin
        if rising_edge(clock) then
            foo := (others=>'0');
            foo := foo + 1;
        end if;

        bar <= foo;
    end process;

end architecture;

