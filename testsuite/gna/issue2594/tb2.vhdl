library ieee;
use ieee.std_logic_1164.all;

entity tb2 is

end entity tb2;

architecture test of tb2 is
    signal  my_signal_s, sig00_s, sig01_s, sig10_s, sig11_s : std_logic_vector(0 to 3) := (others => '0');
    signal my_sel_s : std_logic_vector(0 to 2-1) := (others => '0');
begin
    with my_sel_s select
        my_signal_s <=  sig00_s when "00",
                        sig01_s when "01",
                        sig10_s when "10",
                        sig11_s when "11",
      unaffected when others;

    process
    begin
      sig00_s <= "0001";
      sig01_s <= "0010";
      sig10_s <= "0100";
      sig11_s <= "1000";
      wait for 1 ns;

      my_sel_s <= "00";
      wait for 1 ns;
      assert my_signal_s = sig00_s severity failure;

      my_sel_s <= "UU";
      wait for 1 ns;
      assert my_signal_s = sig00_s severity failure;
      wait;
    end process;
                          
end architecture test;
