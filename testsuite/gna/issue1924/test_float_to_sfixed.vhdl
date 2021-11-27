library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.fixed_float_types.all;

use work.Package_Float32.all;
use work.Package_Fixed.all;

entity test_Float_To_sFixed is
end entity test_Float_To_sFixed;

architecture RTL of test_Float_To_sFixed is

procedure compute_and_show(sign : in std_logic; exponent : in Integer; fraction : in std_logic_vector(23-1 downto 0)) is
    variable s  : std_logic_vector(32-1 downto 0);
    variable f  : float32;
    variable sf : sFixed(4-1 downto -2);
    variable r  : Real;
begin
        s  := sign & std_logic_vector(to_unsigned(exponent, 8)) & fraction;
        f  := to_float(s, f);
        sf := to_sFixed(f, sf, round_style => fixed_truncate);
        r  := to_real(f);

        report "s  : " & to_string(sign) & "."
                       & to_string(to_unsigned(exponent, 8)) & "."
                       & to_string(fraction)
                       & " -> " & to_string(r);
        report "sf : " & to_string(sf);
        report "";
end compute_and_show;


begin

    process
    begin
        compute_and_show('0',   0, "00000000000000000000000");  -- 0
        compute_and_show('0', 123, "00000000000000000000000");  -- 0.0625
        compute_and_show('0', 124, "00000000000000000000000");  -- 0.125
        compute_and_show('0', 125, "00000000000000000000000");  -- 0.25
        compute_and_show('0', 126, "00000000000000000000000");  -- 0.5
        compute_and_show('0', 127, "00000000000000000000000");  -- 1
        compute_and_show('0', 127, "10000000000000000000000");  -- 1.5
        compute_and_show('0', 127, "11000000000000000000000");  -- 1.75
        compute_and_show('0', 127, "11100000000000000000000");  -- 1.875
        compute_and_show('0', 127, "11110000000000000000000");  -- 1.875
        report "------";
        report "";
        compute_and_show('1',   0, "00000000000000000000000");  -- -0
        compute_and_show('1', 123, "00000000000000000000000");  -- -0.0625
        compute_and_show('1', 124, "00000000000000000000000");  -- -0.125
        compute_and_show('1', 125, "00000000000000000000000");  -- -0.25
        compute_and_show('1', 126, "00000000000000000000000");  -- -0.5
        compute_and_show('1', 127, "00000000000000000000000");  -- -1
        compute_and_show('1', 127, "10000000000000000000000");  -- -1.5
        compute_and_show('1', 127, "11000000000000000000000");  -- -1.75
        compute_and_show('1', 127, "11100000000000000000000");  -- -1.875
        compute_and_show('1', 127, "11110000000000000000000");  -- -1.9375
        compute_and_show('1', 127, "11111000000000000000000");  -- -1.9375
        compute_and_show('1', 127, "11111100000000000000000");  -- -1.9375
        compute_and_show('1', 127, "11111110000000000000000");  -- -1.9375
        compute_and_show('1', 127, "11111111000000000000000");  -- -1.9375
        wait;
    end process;

end architecture RTL;
