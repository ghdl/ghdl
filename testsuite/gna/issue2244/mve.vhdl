library ieee;
use ieee.numeric_std.all;
use ieee.std_logic_1164.all;


entity mve is

end mve;

architecture rtl of mve is
    signal c_a : std_logic_vector(11 downto 0) := x"FAE";
    signal c_b : std_logic_vector(11 downto 0) := x"182";

    signal s_expected_vector : std_logic_vector(31 downto 0);
    signal s_resulting_vector : std_logic_vector(31 downto 0);
begin
    -- Compute expected value using intermediate variables for padding
    expected_value : process
        variable v_a_padded : std_logic_vector(15 downto 0);
        variable v_b_padded : std_logic_vector(15 downto 0);
    begin
        v_a_padded := (15 downto 12 => c_a(11), 11 downto 0 => c_a);
        v_b_padded := (15 downto 12 => c_b(11), 11 downto 0 => c_b);

        s_expected_vector <= v_a_padded & v_b_padded;

        wait for 1 ns;

        report "Expected result " & to_hstring(s_expected_vector) severity note;

        wait;
    end process;

    -- Perform the concatenation and the padding in 1 line
    resulting_value : process
    begin
        s_resulting_vector <=
            (15 downto 12 => c_a(11), 11 downto 0 => c_a) &
            (15 downto 12 => c_b(11), 11 downto 0 => c_b);

        wait for 2 ns;

        report "Actual result " & to_hstring(s_resulting_vector) severity note;

        wait;
    end process;

    checker : process
    begin
        wait for 3 ns;
        assert s_resulting_vector = s_expected_vector severity failure;
        wait;
    end process;
end rtl;
