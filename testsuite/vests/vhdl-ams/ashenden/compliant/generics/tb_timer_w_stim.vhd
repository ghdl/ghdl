
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

library IEEE_proposed; use IEEE_proposed.electrical_systems.all;
library IEEE; use IEEE.std_logic_1164.all;

entity tb_timer_w_stim is

end tb_timer_w_stim;

architecture TB_timer_w_stim of tb_timer_w_stim is
    -- Component declarations
    -- Signal declarations
    terminal in_src, rc_ext : electrical;
	signal trig, rst : std_ulogic;
	signal tim_out :  std_ulogic;
begin
    -- Signal assignments
    -- Component instances
    vio : entity work.v_constant(ideal)
        generic map(
            level => 5.0
        )
        port map(
            pos => in_src,
            neg => ELECTRICAL_REF
        );
    R1 : entity work.resistor(simple)
        generic map(
            resistance => 10.0e3
        )
        port map(
            pos => in_src,
            neg => rc_ext
        );
    C1 : entity work.capacitor(ideal)
        generic map(
            cap => 10.0e-6
        )
        port map(
            p1 => rc_ext,
            p2 => electrical_ref
        );
    timer1 : entity work.timer(behavioral)
        generic map(
            threshold => 2.0,
		clamp_on_resistance => 1.0e-3,
		clamp_off_resistance => 1.0e6
        )        
		port map(
      trigger_n => trig,
	reset => rst,
	q => tim_out,
	rc_ext => rc_ext
        );
  -- rst
    P_rst :
    process
    begin

    wait for 0.000 ns;      rst <=  '1';
      
    wait for 1.000 ms;      rst <=  '0';
      
    wait for 100.000 ms;      rst <=  '1';
    
    wait for 1.000 ms;      rst <=  '0';
      
    wait;
    end process;

  -- trig
    P_trig :
    process
    begin
	wait for 0.0 ns;		trig <= '0';
      
    	wait for 5.000 ms;      trig <=  '1';
      
    	wait for 1.0 ms;      trig <=  '0';
    
	wait for 1.0 ms;      trig <=  '1';

   	wait for 40.0 ms;      trig <=  '1';
     
   	wait for 1.0 ms;      trig <=  '0';
    
	wait for 1.0 ms;      trig <=  '1';

  	wait for 40.0 ms;      trig <=  '1';
    
   	wait for 1.0 ms;      trig <=  '0';
    
   	wait for 1.0 ms;      trig <=  '1';
      wait;
    end process;
end TB_timer_w_stim;


