library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test is
end test;

architecture test of test is

    subtype byte is unsigned (7 downto 0);
    type arr_t is array (0 to 1) of byte;
    signal arr: arr_t;

begin

    process is
    begin
        arr <= (B"11111111" others => B"00000000");
        --               ^^^^ no comma
        wait for 1 ns;
        report to_string(arr(0)); -- reports 00000000
        wait;
    end process;

end test;
