library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

--! Configuration data types and structures
package test_pattern_generator_generic_pkg is
    generic(
        PIXEL_PER_LINE_MAX : positive := 640
    );

    --! Redeclared constants so that we don't have to pass them to dependent entities as generics again
    constant C_PIXEL_PER_LINE_MAX : positive         := PIXEL_PER_LINE_MAX;

    --! Constrained types with valid ranges for run time configurable options
    subtype Pixel_Per_Line_t is integer range 2 to PIXEL_PER_LINE_MAX;

    type Test_Pattern_Options_t is record
        PixelPerLine  : Pixel_Per_Line_t;
    end record Test_Pattern_Options_t;

    --! Default options for a test pattern generator that are guaranteed to produce valid output
    constant DEFAULT_OPTIONS : Test_Pattern_Options_t := (
        PixelPerLine  => Pixel_Per_Line_t'low
    );

end package test_pattern_generator_generic_pkg;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test_pattern_generator is
    generic(
        package GENERIC_PKG_INSTANCE is new work.test_pattern_generator_generic_pkg generic map(<>)
    );
    port(
        Clk          : in  std_logic;   -- Clock Signal
        --! Generated image parameters runtime options
        Options      : in  GENERIC_PKG_INSTANCE.Test_Pattern_Options_t
    );

end test_pattern_generator;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture rtl of test_pattern_generator is
    use GENERIC_PKG_INSTANCE.all;
    --! Pixel counter, 0 based up-counter, counts pixels in the current line
    signal pixel_cnt : natural range 0 to GENERIC_PKG_INSTANCE.C_PIXEL_PER_LINE_MAX - 1;

begin
end architecture rtl;


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test_pattern_generator_tb is
end entity test_pattern_generator_tb;

architecture sim of test_pattern_generator_tb is
    constant CLK_PERIOD : time := 10 ns;

    -- Test Pattern Generator Generics
    constant PIXEL_PER_LINE_MAX : integer  := 160;

    -- Instance for the type package for the test pattern generator
    package test_pattern_pkg_inst is new work.test_pattern_generator_generic_pkg generic map(
            PIXEL_PER_LINE_MAX => PIXEL_PER_LINE_MAX
        );
    use test_pattern_pkg_inst.all;

    -- Test Pattern Generator Inputs
    signal clk          : std_logic                                    := '0';
    signal control_sig  : test_pattern_pkg_inst.Test_Pattern_Options_t := test_pattern_pkg_inst.DEFAULT_OPTIONS;
    
begin
    test_pattern_generator_inst_tb : entity work.test_pattern_generator
        generic map(
            GENERIC_PKG_INSTANCE => test_pattern_pkg_inst
        )
        port map(
            Clk          => clk,
            Options      => control_sig
        );

    clk <= not clk after CLK_PERIOD / 2;

    test_runner : process
    begin
        wait for CLK_PERIOD;
    end process test_runner;

end architecture sim;
