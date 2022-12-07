library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

entity reproducer is
--    generic(
--        DUT_IN_DATA_WIDTH : natural
--    );
    port(
        clk : in std_logic;
        reset : in std_logic
    );
end entity;

architecture reproducer_arch of reproducer is
    attribute anyconst  : boolean;
    attribute anyseq    : boolean;

    signal w_dut_in_valid   : std_logic;

    attribute anyseq of w_dut_in_valid  : signal is true;
begin

    default clock is rising_edge(clk);
    a_incr_not_eop: assert always {w_dut_in_valid} |=> {w_dut_in_valid} sync_abort (reset = '1');

end architecture;
