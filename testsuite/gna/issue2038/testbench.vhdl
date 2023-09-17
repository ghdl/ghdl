library ieee;
use ieee.std_logic_1164.all;

library modulation_lib;
use modulation_lib.types_pkg.all;

entity testbench is

end entity testbench;

architecture tb of testbench is

    signal tb_modulator_pwm : std_logic;
    signal tb_modulator_req_modulation : std_logic;
    signal tb_modulator_busy : std_logic;

begin

    inst_modulator : entity modulation_lib.modulator(behavioral)
    port map (
        req_modulation => tb_modulator_req_modulation,
        busy => tb_modulator_busy,
        pwm => tb_modulator_pwm
    );

    proc_stimuli : process
        alias v_modulation_buffer is <<variable .testbench.inst_modulator.symbol_buffer : modulation_lib.symbol_stack_pkg.stack>>;
    begin
        report "Simulation start";
        v_modulation_buffer.push(NARROW_SYMBOL);
        v_modulation_buffer.push(NARROW_SYMBOL);
        v_modulation_buffer.push(WIDE_SYMBOL);
        v_modulation_buffer.push(REGULAR_SYMBOL);
        v_modulation_buffer.push(WIDE_SYMBOL);
        tb_modulator_req_modulation <= '1';
        wait until tb_modulator_busy = '1';
        tb_modulator_req_modulation <= '0';
        wait until tb_modulator_busy = '0';
        report "Simulation complete";
        std.env.finish;

    end process proc_stimuli;

end tb;
