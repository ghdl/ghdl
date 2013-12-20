
-- Copyright (C) 2002 Morgan Kaufmann Publishers, Inc

-- This file is part of VESTs (Vhdl tESTs).

-- VESTs is free software; you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by the
-- Free Software Foundation; either version 2 of the License, or (at
-- your option) any later version. 

-- VESTs is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
-- for more details. 

-- You should have received a copy of the GNU General Public License
-- along with VESTs; if not, write to the Free Software Foundation,
-- Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA 

-- Voltage Pulse Source (Includes Frequency Domain settings)

library ieee;  use ieee.math_real.all;
library ieee_proposed;  use ieee_proposed.electrical_systems.all;

entity src_pulse is
  
  generic ( initial : real := 0.0;     -- initial value
            pulse : real;              -- pulsed value
            ti2p : real;	       -- transition time - initial to pulse
            tp2i : real;	       -- transition time - pulse to initial
            delay : time := 0ms;       -- delay time
            width : time;	       -- duration of pulse (includes ti2p)
            period : time;	       -- period 
            ac_mag : real := 1.0;      -- AC magnitude
            ac_phase : real := 0.0 );  -- AC phase (degrees)

  port ( quantity output : out real );

end entity src_pulse;


architecture ideal of src_pulse is

  -- Declare quantity in frequency domain for AC analysis 
  quantity ac_spec : real spectrum ac_mag, math_2_pi * ac_phase / 360.0;

  -- Signal and constant used in process below  
  signal pulse_signal : real := initial;
  constant low_width: time := period - width;

begin

  if domain = quiescent_domain or domain = time_domain use
    output == pulse_signal'ramp(ti2p, tp2i);
  else
    output == ac_spec;   -- used for frequency (AC) analysis
  end use;

  -- Process to create events on pulse_signal used for rise and fall edges
  proc1 : process
  begin
    wait for delay;
    loop
      pulse_signal <= pulse;
      wait for width;
      pulse_signal <= initial;
      wait for low_width;
    end loop;
  end process;

end architecture ideal;
