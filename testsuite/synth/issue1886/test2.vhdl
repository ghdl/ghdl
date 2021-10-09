library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test2 is
    port (
        crnum_in : in std_ulogic_vector(2 downto 0);
        cr_in    : in std_ulogic_vector(31 downto 0);
        crf_out  : out std_ulogic_vector(3 downto 0)
    );
end entity test2;

architecture behaviour of test2 is
begin
    test_0: process(all)
        variable crnum : integer;
        variable j : integer;
    begin
        crnum := to_integer(unsigned(crnum_in));
        j := crnum * 4;
        crf_out <= cr_in(j + 3 downto j);
    end process;
end architecture behaviour;
