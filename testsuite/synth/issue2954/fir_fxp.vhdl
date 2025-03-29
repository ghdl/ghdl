library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use IEEE.math_real.all;

entity fir_fxp is
    generic (
        Q_SCALE : integer := 12;
        INPUT_WIDTH : integer := 16;
        OUTPUT_WIDTH : integer := 16
    );
    port (
        clk          : in  std_logic; -- Clock signal
        rst          : in  std_logic; -- Reset signal
        input_valid  : in  std_logic; -- Input valid signal
        output_valid : out std_logic; -- Output valid signal
        x : in std_logic_vector(INPUT_WIDTH - 1 downto 0) := (others => '0');
        y : out std_logic_vector(OUTPUT_WIDTH - 1 downto 0) := (others => '0')
    );
end entity fir_fxp;

architecture Behavioral of fir_fxp is

    type tap_delay_array_t is array (0 to 15) of signed(INPUT_WIDTH-1 downto 0);
    signal tap_delay : tap_delay_array_t := (others => (others => '0'));

    -- Define a new type for the coefficient array
    type coeff_array_t is array (0 to 15) of integer;

    -- Declare the coefficient constant using the new type
    constant coeff : coeff_array_t := (
        integer(4.3723023e-003 * 2.0 ** Q_SCALE),
        integer(8.5982873e-003 * 2.0 ** Q_SCALE),
        integer(2.0507030e-002 * 2.0 ** Q_SCALE),
        integer(4.1170253e-002 * 2.0 ** Q_SCALE),
        integer(6.8559790e-002 * 2.0 ** Q_SCALE),
        integer(9.7789676e-002 * 2.0 ** Q_SCALE),
        integer(1.2244394e-001 * 2.0 ** Q_SCALE),
        integer(1.3655872e-001 * 2.0 ** Q_SCALE),
        integer(1.3655872e-001 * 2.0 ** Q_SCALE),
        integer(1.2244394e-001 * 2.0 ** Q_SCALE),
        integer(9.7789676e-002 * 2.0 ** Q_SCALE),
        integer(6.8559790e-002 * 2.0 ** Q_SCALE),
        integer(4.1170253e-002 * 2.0 ** Q_SCALE),
        integer(2.0507030e-002 * 2.0 ** Q_SCALE),
        integer(8.5982873e-003 * 2.0 ** Q_SCALE),
        integer(4.3723023e-003 * 2.0 ** Q_SCALE)
    );

    signal result : signed(OUTPUT_WIDTH + Q_SCALE - 1 downto 0);

begin

    process (clk, rst)
        variable acc : signed(OUTPUT_WIDTH + Q_SCALE - 1 downto 0);
    begin
        if rst = '1' then
            tap_delay <= (others => (others => '0'));
            result <= (others => '0');
            output_valid <= '0';
        elsif rising_edge(clk) then
            if input_valid = '1' then
                acc := (others => '0');
                for ii in 0 to 15 loop
                    acc := acc + shift_right(tap_delay(ii) * coeff(ii), Q_SCALE);
                end loop;
                result <= acc;
                tap_delay(0) <= signed(x);
                for ii in 1 to 15 loop
                    tap_delay(ii) <= tap_delay(ii-1);
                end loop;
                output_valid <= '1';
            else
                output_valid <= '0';
            end if;
        end if;
    end process;

    y <= std_logic_vector(result(OUTPUT_WIDTH + Q_SCALE - 1 downto Q_SCALE));

end architecture Behavioral;
