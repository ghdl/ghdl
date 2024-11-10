--! Configuration data types and structure
package generic_pkg is
    generic(
        WIDTH_MAX: positive:= 640
    );

    -- Subtype declaration, seems to be the source of the crash
    subtype Prng_Seed_t is bit_vector(WIDTH_MAX downto 0);

    type Test_Pattern_Options_t is record
        PRNGSeed         : Prng_Seed_t;
    end record Test_Pattern_Options_t;
end package generic_pkg;

use work.generic_pkg;

-- Entity with instance of generic package as generic
entity tpg is
    generic(package GENERIC_PKG_INSTANCE is new work.generic_pkg generic map(<>));
    port(pattern_options : in  GENERIC_PKG_INSTANCE.Test_Pattern_Options_t);
end tpg;

architecture rtl of tpg is
begin
end architecture;

use std.env.finish;
use work.generic_pkg;
use work.tpg;

entity mini_repro is
end entity;

architecture sim of mini_repro is
    -- Instanciate the generic package
    package generic_pkg_inst is new generic_pkg generic map(WIDTH_MAX => 3);
    
    -- Signal with a type from the generic package
    signal pattern_options : generic_pkg_inst.Test_Pattern_Options_t;
begin

    -- Entity with the generic package as generic
DUT_INST : entity work.tpg
    generic map(
        GENERIC_PKG_INSTANCE => generic_pkg_inst
    )
    port map(
        pattern_options => pattern_options
    );

test_runner : process
    variable test_var : generic_pkg_inst.Test_Pattern_Options_t;
begin
    finish;
end process test_runner;
end architecture;
