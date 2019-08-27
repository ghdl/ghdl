library ieee;
use ieee.std_logic_1164.all;

entity testcase is
        port(
                data_in  : in std_ulogic;
                data_out : out std_ulogic
        );
end entity testcase;

architecture behaviour of testcase is
begin
        comb : process(all)
        begin
                data_out <= '1' when data_in = '0' else '0';
        end process;
end architecture behaviour;
