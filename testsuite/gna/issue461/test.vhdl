library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test is
end test;

architecture behaviour of test is
    signal selector : std_logic_vector(1 downto 0) := "UU";
    signal result   : std_logic_vector(7 downto 0);

    signal op_1     : std_logic_vector(7 downto 0);
    signal op_2     : std_logic_vector(7 downto 0);
begin
    with selector select
        result <= (std_logic_vector(signed(op_1) + signed(op_2))) when "00",
                  (others => '-') when others;
end behaviour;
