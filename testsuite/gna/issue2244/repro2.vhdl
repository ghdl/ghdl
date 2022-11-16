library ieee;
use ieee.std_logic_1164.all;

entity repro2 is
end repro2;

architecture rtl of repro2 is
    signal c_a : std_logic_vector(11 downto 0) := x"FAE";
    signal c_b : std_logic_vector(11 downto 0) := x"182";
begin
    expected_value : process
        variable v_a_padded : std_logic_vector(15 downto 0);
        variable v_b_padded : std_logic_vector(15 downto 0);
        variable s_expected_vector : std_logic_vector(31 downto 0);
        variable s_resulting_vector : std_logic_vector(31 downto 0);
    begin
        v_a_padded := (15 downto 12 => c_a(11), 11 downto 0 => c_a);
        v_b_padded := (15 downto 12 => c_b(11), 11 downto 0 => c_b);

        s_expected_vector := v_a_padded & v_b_padded;
        report "Expected result " & to_hstring(s_expected_vector) severity note;

        s_resulting_vector :=
          (15 downto 12 => c_a(11), 11 downto 0 => c_a) &
          (15 downto 12 => c_b(11), 11 downto 0 => c_b);
        report "Actual result " & to_hstring(s_resulting_vector) severity note;

        assert s_resulting_vector = s_expected_vector severity error;

        s_resulting_vector :=
          v_a_padded & (15 downto 12 => c_b(11), 11 downto 0 => c_b);
        report "Actual result " & to_hstring(s_resulting_vector) severity note;

        assert s_resulting_vector = s_expected_vector severity error; --failure;
        wait;
    end process;
end rtl;
