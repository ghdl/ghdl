library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test2 is
    port(
        d_in: in std_ulogic_vector(63 downto 0);
        d_out: out std_ulogic_vector(63 downto 0)
        );
end entity test2;

architecture behaviour of test2 is
begin
    comb : process(all)
    begin
        d_out <= std_logic_vector(unsigned(d_in) + 4);
    end process;
end architecture behaviour;
