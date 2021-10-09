library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test3 is
    port (
        crnum_in : in std_ulogic_vector(2 downto 0);
        cr_in    : in std_ulogic_vector(0 to 31);
        crf_out  : out std_ulogic_vector(3 downto 0)
    );
end;

architecture behaviour of test3 is
begin
    test_0: process(all)
        variable crnum : integer;
    begin
        crnum := to_integer(unsigned(crnum_in));
        crf_out <= cr_in(crnum * 4 to crnum * 4 + 3);
    end process;
end architecture behaviour;
