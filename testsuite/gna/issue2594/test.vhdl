library ieee;
use ieee.std_logic_1164.all;

entity selsigass_tb is

end entity selsigass_tb;

architecture test of selsigass_tb is
    signal  my_signal_s, sig00_s, sig01_s, sig10_s, sig11_s : std_logic_vector(0 to 32-1) := (others => '0');
    signal my_sel_s : std_logic_vector(0 to 2-1) := (others => '0');
begin
    with my_sel_s select
        my_signal_s <=  sig00_s when "00",
                        sig01_s when "01",
                        sig10_s when "10",
                        sig11_s when "11",
                        unaffected when others;
end architecture test;
