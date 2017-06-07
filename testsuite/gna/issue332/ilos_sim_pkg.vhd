
Library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package ilos_sim_pkg is

procedure clk_gen(
	signal clk : out std_logic; 
	constant FREQ : real; PHASE : time := 0 fs; 
	signal run : std_logic
	);

end package ilos_sim_pkg;


package body ilos_sim_pkg is

-- Advanced procedure for clock generation, with period adjust to match frequency over time, and run control by signal
procedure clk_gen(signal clk : out std_logic; constant FREQ : real; PHASE : time := 0 fs; signal run : std_logic) is
  constant HIGH_TIME   : time := 0.5 sec / FREQ;  -- High time as fixed value
  variable low_time_v  : time;                    -- Low time calculated per cycle; always >= HIGH_TIME
  variable cycles_v    : real := 0.0;             -- Number of cycles
  variable freq_time_v : time := 0 fs;            -- Time used for generation of cycles
begin
  -- Check the arguments
  assert (HIGH_TIME /= 0 fs) report "clk_gen: High time is zero; time resolution to large for frequency" severity FAILURE;
  -- Initial phase shift
  clk <= '0';
  wait for PHASE;
  -- Generate cycles
  loop
    -- Only high pulse if run is '1' or 'H'
    if (run = '1') or (run = 'H') then
      clk <= run;
    end if;
    wait for HIGH_TIME;
    -- Low part of cycle
    clk <= '0';
    low_time_v := 1 sec * ((cycles_v + 1.0) / FREQ) - freq_time_v - HIGH_TIME;  -- + 1.0 for cycle after current
    wait for low_time_v;
    -- Cycle counter and time passed update
    cycles_v := cycles_v + 1.0;
    freq_time_v := freq_time_v + HIGH_TIME + low_time_v;
  end loop;
end procedure;


end package body ilos_sim_pkg;