library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test2 is
    port (
        d_in: in std_logic_vector(1 downto 0);
        d_out: out std_logic_vector(1 downto 0)
        );
end entity test2;

architecture rtl of test2 is
    constant c : std_logic_vector (7 downto 0) := "10010000";
begin
    d_out <= c(to_integer(unsigned(d_in))+1 downto to_integer(unsigned(d_in)));
end rtl;
