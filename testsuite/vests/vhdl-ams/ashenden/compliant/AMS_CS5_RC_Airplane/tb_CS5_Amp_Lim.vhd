
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

library IEEE;
library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
use IEEE_proposed.mechanical_systems.all;

entity sum2_e is
	generic (k1, k2: real := 1.0);  -- Gain multipliers
	port 	(	terminal in1, in2: electrical;
			terminal output: electrical);
end entity sum2_e;

architecture simple of sum2_e is
  QUANTITY vin1 ACROSS in1 TO ELECTRICAL_REF;
  QUANTITY vin2 ACROSS in2 TO ELECTRICAL_REF;
  QUANTITY vout ACROSS iout THROUGH output TO ELECTRICAL_REF;

begin
	vout == k1*vin1 + k2*vin2;
end architecture simple;
--

library IEEE;
use IEEE.MATH_REAL.all;
-- Use proposed IEEE natures and packages
library IEEE_proposed;
use IEEE_proposed.ELECTRICAL_SYSTEMS.all;

entity gain_e is
	generic (
		k: REAL := 1.0);  -- Gain multiplier
	port 	(	terminal input : electrical;
			terminal output: electrical);
end entity gain_e;

architecture simple of gain_e is

  QUANTITY vin ACROSS input TO ELECTRICAL_REF;
  QUANTITY vout ACROSS iout THROUGH output TO ELECTRICAL_REF;

begin
	vout == k*vin;
end architecture simple;
--

-------------------------------------------------------------------------------
-- S-Domain Limiter Model
--
-------------------------------------------------------------------------------

library IEEE_proposed; use IEEE_proposed.electrical_systems.all;
entity limiter_2_e is
	generic (
		limit_high : real := 4.8;	-- upper limit
		limit_low : real := -4.8); 	-- lower limit
	port (
		terminal input: electrical;
		terminal output: electrical);					
end entity limiter_2_e;

architecture simple of limiter_2_e is
  	QUANTITY vin ACROSS input TO ELECTRICAL_REF;
  	QUANTITY vout ACROSS iout THROUGH output TO ELECTRICAL_REF;
	constant slope : real := 1.0e-4;
begin
	if vin > limit_high use	-- Upper limit exceeded, so limit input signal
		vout == limit_high + slope*(vin - limit_high);
	elsif vin < limit_low use	-- Lower limit exceeded, so limit input signal
		vout == limit_low + slope*(vin - limit_low);
	else		-- No limit exceeded, so pass input signal as is
		vout == vin;
	end use;
	break on vin'above(limit_high), vin'above(limit_low);
end architecture simple;

--

-------------------------------------------------------------------------------
-- Lead-Lag Filter
--
--  Transfer Function:
--
--                  (s + w1)         
--    H(s) =  k *  ----------
--                  (s + w2)
--
-- DC Gain = k*w1/w2
-------------------------------------------------------------------------------

-- Use IEEE_proposed instead of disciplines
library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
library IEEE;
use ieee.math_real.all;

entity lead_lag_e is
	generic (
		k: real := 1.0;  			-- Gain multiplier
		f1: real := 10.0;			-- First break frequency (zero)
		f2: real := 100.0);		-- Second break frequency (pole)
	port 	(	terminal input: electrical;
			terminal output: electrical);
end entity lead_lag_e;

architecture simple of lead_lag_e is
  QUANTITY vin ACROSS input TO ELECTRICAL_REF;
  QUANTITY vout ACROSS iout THROUGH output TO ELECTRICAL_REF;

	quantity vin_temp : real;
	constant w1 : real := f1*math_2_pi;
	constant w2 : real := f2*math_2_pi;
	constant num : real_vector := (w1, 1.0);                                                    
	constant den : real_vector := (w2, 1.0);
begin
	vin_temp == vin;
	vout == k*vin_temp'ltf(num, den);
end architecture simple;

--
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;

library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
use IEEE_proposed.mechanical_systems.all;

entity rudder_servo is
    port(
        terminal servo_in : electrical;
        terminal pos_fb : electrical;
        terminal servo_out : electrical
    );
end rudder_servo;

architecture rudder_servo of rudder_servo is
    -- Component declarations
    -- Signal declarations
    terminal error : electrical;
    terminal ll_in : electrical;
    terminal ll_out : electrical;
    terminal summer_fb : electrical;
begin
    -- Signal assignments
    -- Component instances
    summer : entity work.sum2_e(simple)
        port map(
            in1 => servo_in,
            in2 => summer_fb,
            output => error
        );
    forward_gain : entity work.gain_e(simple)
        generic map(
            k => 100.0
        )
        port map(
            input => error,
            output => ll_in
        );
    fb_gain : entity work.gain_e(simple)
        generic map(
            k => -4.57
        )
        port map(
            input => pos_fb,
            output => summer_fb
        );
    XCMP21 : entity work.limiter_2_e(simple)
          generic map(
            limit_high => 4.8,
            limit_low => -4.8
        )
        port map(
            input => ll_out,
            output => servo_out
        );
    XCMP22 : entity work.lead_lag_e(simple)
        generic map(
            f2 => 2000.0,
            f1 => 5.0,
            k => 400.0
        )
        port map(
            input => ll_in,
            output => ll_out
        );
end rudder_servo;
--

-------------------------------------------------------------------------------
-- Copyright (c) 2001 Mentor Graphics Corporation
--
-- This model is a component of the Mentor Graphics VHDL-AMS educational open 
-- source model library, and is covered by this license agreement. This model,
-- including any updates, modifications, revisions, copies, and documentation 
-- are copyrighted works of Mentor Graphics. USE OF THIS MODEL INDICATES YOUR 
-- COMPLETE AND UNCONDITIONAL ACCEPTANCE OF THE TERMS AND CONDITIONS SET FORTH
-- IN THIS LICENSE AGREEMENT.  Mentor Graphics grants you a non-exclusive 
-- license to use, reproduce, modify and distribute this model, provided that:
-- (a) no fee or other consideration is charged for any distribution except 
-- compilations distributed in accordance with Section (d) of this license 
-- agreement; (b) the comment text embedded in this model is included verbatim
-- in each copy of this model made or distributed by you, whether or not such 
-- version is modified; (c) any modified version must include a conspicuous 
-- notice that this model has been modified and the date of modification; and 
-- (d) any compilations sold by you that include this model must include a 
-- conspicuous notice that this model is available from Mentor Graphics in its
-- original form at no charge.
--
-- THIS MODEL IS LICENSED TO YOU "AS IS" AND WITH NO WARRANTIES, EXPRESS OR 
-- IMPLIED.  MENTOR GRAPHICS SPECIFICALLY DISCLAIMS ALL IMPLIED WARRANTIES OF 
-- MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  MENTOR GRAPHICS SHALL
-- HAVE NO RESPONSIBILITY FOR ANY DAMAGES WHATSOEVER.
-------------------------------------------------------------------------------
-- File       : gear_rv_r.vhd
-- Author     : Mentor Graphics
-- Created    : 2001/10/10
-- Last update: 2019-12-30
-------------------------------------------------------------------------------
-- Description: Gear Model (ROTATIONAL_V/ROTATIONAL domains)
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version     Author              Description
-- 2001/10/10  1.0         Mentor Graphics     Created    
-------------------------------------------------------------------------------

-- Use proposed IEEE natures and packages
library IEEE_proposed;
use IEEE_proposed.mechanical_systems.all;

entity gear_rv_r is

  generic(
    ratio : real := 1.0);   -- Gear ratio (Revs of shaft2 for 1 rev of shaft1)
                            -- Note: can be negative, if shaft polarity changes

  port ( terminal rotv1 : rotational_v;
  		terminal rot2 : rotational);

end entity gear_rv_r;

-------------------------------------------------------------------------------
-- Ideal Architecture
-------------------------------------------------------------------------------
architecture ideal of gear_rv_r is

  quantity w1 across torq_vel through rotv1 to rotational_v_ref;
--  quantity w2 across torq2 through rotv2 to rotational_v_ref;
  quantity theta across torq_ang through rot2 to rotational_ref;

begin

--  w2  == w1*ratio;
  theta == ratio*w1'integ;
  torq_vel == -1.0*torq_ang*ratio;

end architecture ideal;

-------------------------------------------------------------------------------
-- Copyright (c) 2001 Mentor Graphics Corporation
-------------------------------------------------------------------------------
--

-------------------------------------------------------------------------------
-- Rotational to Electrical Converter
--
-------------------------------------------------------------------------------

-- Use IEEE_proposed instead of disciplines
library IEEE;
use ieee.math_real.all;
library IEEE_proposed;
use IEEE_proposed.mechanical_systems.all;
use IEEE_proposed.electrical_systems.all;

entity rot2v is
  
  generic (
    k : real := 1.0);                -- optional gain

  port (
    terminal input   : rotational;  -- input terminal
    terminal output    : electrical);   -- output terminal
  
end entity rot2v ;

architecture bhv of rot2v is
quantity rot_in across input to rotational_ref;     -- Converter's input branch
quantity v_out across out_i through output to electrical_ref;-- Converter's output branch

  begin  -- bhv
   v_out ==  k*rot_in; 
end bhv;
--

-------------------------------------------------------------------------------
-- Control Horn for Rudder Control (mechanical implementation)
--
--  Transfer Function:
--                            
--  tran =   R*sin(rot) 
--
-- Where pos = output translational position,
-- R = horn radius, 
-- theta = input rotational angle
-------------------------------------------------------------------------------

-- Use IEEE_proposed instead of disciplines
library IEEE;
use ieee.math_real.all;
library IEEE_proposed;
use IEEE_proposed.mechanical_systems.all;

entity horn_r2t is
  
  generic (
    R   : real := 1.0);            		-- horn radius

  port (
    terminal theta    : ROTATIONAL;   	-- input angular position port
    terminal pos   : TRANSLATIONAL);  	-- output translational position port
     
end entity horn_r2t;

architecture bhv of horn_r2t is

  QUANTITY rot across rot_tq through theta TO ROTATIONAL_REF;
  QUANTITY tran across tran_frc through pos TO TRANSLATIONAL_REF;

  begin			-- bhv
   tran == R*sin(rot);		-- Convert angle in to translational out
   tran_frc == -rot_tq/R;	-- Convert torque in to force out 
end bhv;
--

-------------------------------------------------------------------------------
-- Control Horn for Rudder Control (mechanical implementation)
--
--  Transfer Function:
--                            
--  theta =   arcsin(pos/R) 
--
-- Where pos = input translational position,
-- R = horn radius, 
-- theta = output rotational angle
-------------------------------------------------------------------------------

-- Use IEEE_proposed instead of disciplines
library IEEE;
use ieee.math_real.all;
library IEEE_proposed;
use IEEE_proposed.mechanical_systems.all;

entity horn_t2r is
  
  generic (
    R   : real := 1.0);            -- Rudder horn radius

  port (
    terminal pos    : translational;   -- input translational position port
    terminal theta   : rotational);  -- output angular position port
     
end entity horn_t2r ;

architecture bhv of horn_t2r is

  QUANTITY tran across tran_frc through pos TO TRANSLATIONAL_REF;
  QUANTITY rot across rot_tq through theta TO ROTATIONAL_REF;

  begin  -- bhv
   rot == arcsin(tran/R);	-- Convert translational to angle
   rot_tq == -tran_frc*R;	-- Convert force to torque
   
end bhv;
--

-------------------------------------------------------------------------------
-- Copyright (c) 2001 Mentor Graphics Corporation
--
-- This model is a component of the Mentor Graphics VHDL-AMS educational open 
-- source model library, and is covered by this license agreement. This model,
-- including any updates, modifications, revisions, copies, and documentation 
-- are copyrighted works of Mentor Graphics. USE OF THIS MODEL INDICATES YOUR 
-- COMPLETE AND UNCONDITIONAL ACCEPTANCE OF THE TERMS AND CONDITIONS SET FORTH
-- IN THIS LICENSE AGREEMENT.  Mentor Graphics grants you a non-exclusive 
-- license to use, reproduce, modify and distribute this model, provided that:
-- (a) no fee or other consideration is charged for any distribution except 
-- compilations distributed in accordance with Section (d) of this license 
-- agreement; (b) the comment text embedded in this model is included verbatim
-- in each copy of this model made or distributed by you, whether or not such 
-- version is modified; (c) any modified version must include a conspicuous 
-- notice that this model has been modified and the date of modification; and 
-- (d) any compilations sold by you that include this model must include a 
-- conspicuous notice that this model is available from Mentor Graphics in its
-- original form at no charge.
--
-- THIS MODEL IS LICENSED TO YOU "AS IS" AND WITH NO WARRANTIES, EXPRESS OR 
-- IMPLIED.  MENTOR GRAPHICS SPECIFICALLY DISCLAIMS ALL IMPLIED WARRANTIES OF 
-- MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  MENTOR GRAPHICS SHALL
-- HAVE NO RESPONSIBILITY FOR ANY DAMAGES WHATSOEVER.
-------------------------------------------------------------------------------
-- File       : DC_Motor.vhd
-- Author     : Mentor Graphics
-- Created    : 2001/06/16
-- Last update: 2001/06/16
-------------------------------------------------------------------------------
-- Description: Basic DC Motor
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version     Author              Description
-- 2001/06/16  1.0         Mentor Graphics     Created    
-------------------------------------------------------------------------------

-- Use proposed IEEE natures and packages
library IEEE_proposed;
use IEEE_proposed.mechanical_systems.all;
use IEEE_proposed.electrical_systems.all;

entity DC_Motor is

  generic (
    r_wind : resistance;                -- Motor winding resistance [Ohm]
    kt     : real;                      -- Torque coefficient [N*m/Amp]
    l      : inductance;                -- Winding inductance [Henrys]
    d      : real;                      -- Damping coefficient [N*m/(rad/sec)]
    j      : mmoment_i);                -- Moment of inertia [kg*meter**2]

  port (terminal p1, p2 : electrical;
        terminal shaft_rotv : rotational_v);

end entity DC_Motor;

-------------------------------------------------------------------------------
-- Basic Architecture
-- Motor equations:  V = Kt*W + I*Rwind + L*dI/dt
--                   T = -Kt*I + D*W + J*dW/dt
-------------------------------------------------------------------------------
architecture basic of DC_Motor is

  quantity v across i through p1 to p2;
  quantity w across torq through shaft_rotv to rotational_v_ref;

begin

  torq == -1.0*kt*i + d*w + j*w'dot;
  v  == kt*w + i*r_wind + l*i'dot;

end architecture basic;

-------------------------------------------------------------------------------
-- Copyright (c) 2001 Mentor Graphics Corporation
-------------------------------------------------------------------------------
--

-------------------------------------------------------------------------------
-- Copyright (c) 2001 Mentor Graphics Corporation
--
-- This model is a component of the Mentor Graphics VHDL-AMS educational open 
-- source model library, and is covered by this license agreement. This model,
-- including any updates, modifications, revisions, copies, and documentation 
-- are copyrighted works of Mentor Graphics. USE OF THIS MODEL INDICATES YOUR 
-- COMPLETE AND UNCONDITIONAL ACCEPTANCE OF THE TERMS AND CONDITIONS SET FORTH
-- IN THIS LICENSE AGREEMENT.  Mentor Graphics grants you a non-exclusive 
-- license to use, reproduce, modify and distribute this model, provided that:
-- (a) no fee or other consideration is charged for any distribution except 
-- compilations distributed in accordance with Section (d) of this license 
-- agreement; (b) the comment text embedded in this model is included verbatim
-- in each copy of this model made or distributed by you, whether or not such 
-- version is modified; (c) any modified version must include a conspicuous 
-- notice that this model has been modified and the date of modification; and 
-- (d) any compilations sold by you that include this model must include a 
-- conspicuous notice that this model is available from Mentor Graphics in its
-- original form at no charge.
--
-- THIS MODEL IS LICENSED TO YOU "AS IS" AND WITH NO WARRANTIES, EXPRESS OR 
-- IMPLIED.  MENTOR GRAPHICS SPECIFICALLY DISCLAIMS ALL IMPLIED WARRANTIES OF 
-- MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  MENTOR GRAPHICS SHALL
-- HAVE NO RESPONSIBILITY FOR ANY DAMAGES WHATSOEVER.
-------------------------------------------------------------------------------
-- File       : stop_r.vhd
-- Author     : Mentor Graphics
-- Created    : 2001/10/10
-- Last update: 2001/10/10
-------------------------------------------------------------------------------
-- Description: Mechanical Hard Stop (ROTATIONAL domain)
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version     Author              Description
-- 2001/06/16  1.0         Mentor Graphics     Created    
-------------------------------------------------------------------------------

library IEEE;
use IEEE.MATH_REAL.all;

-- Use proposed IEEE natures and packages
library IEEE_proposed;
use IEEE_proposed.MECHANICAL_SYSTEMS.all;


entity stop_r is

  generic (
    k_stop    : real;
--    ang_max   : angle;
--    ang_min   : angle := 0.0;
	ang_max   : real;
    ang_min   : real := 0.0;
    damp_stop : real  := 0.000000001
    );

  port ( terminal ang1, ang2 : rotational);

end entity stop_r;

architecture ideal of stop_r is

  quantity velocity : velocity;
  quantity ang across trq through ang1 to ang2;

begin

  velocity == ang'dot;

  if ang'above(ang_max) use
    trq == k_stop * (ang - ang_max) + (damp_stop * velocity);
  elsif ang'above(ang_min) use
    trq   == 0.0;
  else
    trq   == k_stop * (ang - ang_min) + (damp_stop * velocity);
  end use;

break on ang'above(ang_min), ang'above(ang_max);

end architecture ideal;

-------------------------------------------------------------------------------
-- Copyright (c) 2001 Mentor Graphics Corporation
-------------------------------------------------------------------------------
--

library IEEE;
use IEEE.std_logic_arith.all;
library IEEE_proposed;
use IEEE_proposed.mechanical_systems.all;

entity tran_linkage is
port
(
      terminal p1, p2 : translational
);

begin

end tran_linkage;

architecture a1 of tran_linkage is

  QUANTITY pos_1 across frc_1 through p1 TO translational_ref;
  QUANTITY pos_2 across frc_2 through p2 TO translational_ref;

begin

   pos_2 == pos_1;		-- Pass position 
   frc_2 == -frc_1;	-- Pass force

end;
--

-------------------------------------------------------------------------------
-- Rudder Model (Rotational Spring)
--
--  Transfer Function:
--                            
--  torq =   -k*(theta - theta_0)
--
-- Where theta = input rotational angle,
-- torq = output rotational angle,
-- theta_0 = reference angle
-------------------------------------------------------------------------------

-- Use IEEE_proposed instead of disciplines
library IEEE;
use ieee.math_real.all;
library IEEE_proposed;
use IEEE_proposed.mechanical_systems.all;

entity rudder is
  
  generic (
    k   : real := 1.0;            -- Spring constant
	theta_0 : real := 0.0);

  port (
    terminal rot    : rotational);   -- input rotational angle
     
end entity rudder;

architecture bhv of rudder is

  QUANTITY theta across torq through rot TO ROTATIONAL_REF;

  begin  -- bhv

   torq == k*(theta - theta_0);	-- Convert force to torque
   
end bhv;
--

-- Copyright Mentor Graphics Corporation 2001
-- Confidential Information Provided Under License Agreement for Internal Use Only

-- Electrical Resistor Model

-- Use proposed IEEE natures and packages
LIBRARY IEEE_proposed;
USE IEEE_proposed.ELECTRICAL_SYSTEMS.ALL;

ENTITY resistor IS 

-- Initialize parameters
  GENERIC (
    res : RESISTANCE);                 -- resistance (no initial value)

-- Define ports as electrical terminals
  PORT (
    TERMINAL p1, p2 : ELECTRICAL);

END ENTITY resistor;

-- Ideal Architecture (V = I*R)
ARCHITECTURE ideal OF resistor IS
  
-- Declare Branch Quantities
  QUANTITY v ACROSS i THROUGH p1 TO p2;

BEGIN
  
-- Characteristic equations
  v == i*res;                       

END ARCHITECTURE ideal;

--
library ieee_proposed;  
use ieee_proposed.electrical_systems.all;

entity amp_lim is
		port (terminal ps : electrical; -- positive supply terminal
			terminal input, output : electrical);
end entity amp_lim;


architecture simple of amp_lim is
	quantity v_pwr across i_pwr through ps to electrical_ref;
	quantity vin across iin through input to electrical_ref;
	quantity vout across iout through output to electrical_ref;
	quantity v_amplified : voltage ;
	constant gain : real := 1.0;
begin
	v_amplified == gain*vin;
	
	if v_amplified > v_pwr use 
		vout == v_pwr;
	else 
		vout == v_amplified;
	end use;	
	
	-- ignore loading effects
	i_pwr == 0.0;
	iin  == 0.0;

end architecture simple;
--

-------------------------------------------------------------------------------
-- Copyright (c) 2001 Mentor Graphics Corporation
--
-- This model is a component of the Mentor Graphics VHDL-AMS educational open 
-- source model library, and is covered by this license agreement. This model,
-- including any updates, modifications, revisions, copies, and documentation 
-- are copyrighted works of Mentor Graphics. USE OF THIS MODEL INDICATES YOUR 
-- COMPLETE AND UNCONDITIONAL ACCEPTANCE OF THE TERMS AND CONDITIONS SET FORTH
-- IN THIS LICENSE AGREEMENT.  Mentor Graphics grants you a non-exclusive 
-- license to use, reproduce, modify and distribute this model, provided that:
-- (a) no fee or other consideration is charged for any distribution except 
-- compilations distributed in accordance with Section (d) of this license 
-- agreement; (b) the comment text embedded in this model is included verbatim
-- in each copy of this model made or distributed by you, whether or not such 
-- version is modified; (c) any modified version must include a conspicuous 
-- notice that this model has been modified and the date of modification; and 
-- (d) any compilations sold by you that include this model must include a 
-- conspicuous notice that this model is available from Mentor Graphics in its
-- original form at no charge.
--
-- THIS MODEL IS LICENSED TO YOU "AS IS" AND WITH NO WARRANTIES, EXPRESS OR 
-- IMPLIED.  MENTOR GRAPHICS SPECIFICALLY DISCLAIMS ALL IMPLIED WARRANTIES OF 
-- MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  MENTOR GRAPHICS SHALL
-- HAVE NO RESPONSIBILITY FOR ANY DAMAGES WHATSOEVER.
-------------------------------------------------------------------------------
-- File       : v_pulse.vhd
-- Author     : Mentor Graphics
-- Created    : 2001/06/16
-- Last update: 2001/07/09
-------------------------------------------------------------------------------
-- Description:  Voltage Pulse Source
--               Includes Frequency Domain settings
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version     Author              Description
-- 2001/06/16  1.0         Mentor Graphics     Created
-- 2001/07/09  1.1         Mentor Graphics     Changed input parameters to type
--                                             time.  Uses time2real function.
--                                             Pulsewidth no longer includes
--                                             rise and fall times.
-------------------------------------------------------------------------------

library IEEE;
use IEEE.MATH_REAL.all;
-- Use proposed IEEE natures and packages
library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;

entity v_pulse is

  generic (
    initial  : voltage := 0.0;          -- initial value [Volts]
    pulse    : voltage;                 -- pulsed value [Volts]
    ti2p     : time    := 1 ns;          -- initial to pulse [Sec]
    tp2i     : time    := 1 ns;          -- pulse to initial [Sec]
    delay    : time    := 0 ms;          -- delay time [Sec]
    width    : time;                    -- duration of pulse [Sec]
    period   : time;                    -- period [Sec]
    ac_mag   : voltage := 1.0;          -- AC magnitude [Volts]
    ac_phase : real    := 0.0);         -- AC phase [Degrees]

  port (
    terminal pos, neg : electrical);

end entity v_pulse;

-------------------------------------------------------------------------------
-- Ideal Architecture
-------------------------------------------------------------------------------
architecture ideal of v_pulse is

-- Declare Through and Across Branch Quantities
  quantity v across i through pos to neg;
-- Declare quantity in frequency domain for AC analysis 
  quantity ac_spec      : real spectrum ac_mag, math_2_pi*ac_phase/360.0;
-- Signal used in CreateEvent process below  
  signal   pulse_signal : voltage := initial;
  
-- Convert ti2p and tp2i generics to type REAL (needed for 'RAMP attribute)
-- Note: these lines gave an error during simulation.  Had to use a
-- function call instead.
--  constant ri2p : real := time'pos(ti2p) * 1.0e-15;
--  constant rp2i : real := time'pos(tp2i) * 1.0e-15;
  
-- Function to convert numbers of type TIME to type REAL
  function time2real(tt : time) return real is
  begin
    return time'pos(tt) * 1.0e-15;
  end time2real;
-- Convert ti2p and tp2i generics to type REAL (needed for 'RAMP attribute)
  constant ri2p         : real    := time2real(ti2p);
  constant rp2i         : real    := time2real(tp2i);

begin

  if domain = quiescent_domain or domain = time_domain use
    v == pulse_signal'ramp(ri2p, rp2i);  -- create rise and fall transitions
  else
    v   == ac_spec;                        -- used for Frequency (AC) analysis
  end use;

-- purpose: Create events to define pulse shape
-- type   : combinational
-- inputs : 
-- outputs: pulse_signal
CreateEvent : process
begin
  wait for delay;
  loop
    pulse_signal <= pulse;
    wait for (width + ti2p);
    pulse_signal <= initial;
    wait for (period - width - ti2p);
  end loop;
end process CreateEvent;

end architecture ideal;

-------------------------------------------------------------------------------
-- Copyright (c) 2001 Mentor Graphics Corporation
-------------------------------------------------------------------------------
--

library ieee;
use ieee.math_real.all;
package pwl_full_functions is

	function next_increment(x : in real; xdata : in real_vector ) 
		return real;
	function interpolate (x,y2,y1,x2,x1 : in real) 
		return real;
	function pwl_dim1_flat (x : in real; xdata, ydata : in real_vector ) 
		return real;

end package pwl_full_functions;

package body pwl_full_functions is

	function next_increment(x : in real; xdata : in real_vector) 
		return real is 
		variable i : integer;
	begin
		i := 0;
		while i <= xdata'right loop
	    	if x >= xdata(i) - 6.0e-15 then   -- The value 6.0e-15 envelopes round-off error 
			                                  -- of real-to-time conversion in calling model
				i := i + 1;
			else
				return xdata(i) - xdata(i - 1);
            end if;  
        end loop;
 	    return 1.0;  -- Returns a "large number" relative to expected High-Speed time scale
	end function next_increment;

	function interpolate (x,y2,y1,x2,x1 : in real) 
		return real is 
		variable m, yvalue : real;
	 begin
	 	assert (x1 /= x2)
			report "interpolate: x1 cannot be equal to x2"
			severity error;
		assert (x >= x1) and (x <= x2) 
			report "interpolate: x must be between x1 and x2, inclusively "
			severity error;
	
		m := (y2 - y1)/(x2 - x1);
		yvalue := y1 + m*(x - x1);
		return yvalue;
	end function interpolate;

 	    --  Created a new pwl_dim1_flat function that returns a constant 
		-- value of ydata(0) if  x < xdata(0), or ydata(ydata'right) if x > xdata(xdata'right)

	function pwl_dim1_flat (x : in real; xdata, ydata : in real_vector ) 
		return real is 
		variable xvalue, yvalue, m : real;
                variable start, fin, mid: integer; 
	begin
		if x >= xdata(xdata'right) then
			yvalue := ydata(ydata'right);
			return yvalue;
		end if;
		if x <= xdata(0) then
			yvalue := ydata(0);
			return yvalue;
		end if;
                start:=0;
                fin:=xdata'right;
-- I assume that the valid elements are from  xdata(0) to xdata(fin), inclusive. 
-- so fin==n-1 in C terms (where n is the size of the array). 
		while  start <=fin  loop
                        mid:=(start+fin)/2; 
	                if xdata(mid) < x
                         then start:=mid+1;
                         else fin:=mid-1;
                    end if;  
                end loop; 
                                
                if xdata(mid) > x
                        then mid:=mid-1; 
                 end if; 
                 yvalue := interpolate(x,ydata(mid+1),ydata(mid),xdata(mid+1),xdata(mid));               
		
		return yvalue;
	end function pwl_dim1_flat;

end package body pwl_full_functions;

-- Not sure the sync_tdata process is necessary. Requires the tdata set contain
-- a larger value than the actual simulation time.
--  Piece-wise linear voltage source model

library IEEE;
use IEEE.std_logic_1164.all;
Library IEEE_proposed;
use IEEE_proposed.electrical_systems.all; 
use work.pwl_full_functions.all;
 
entity v_pwl_full is
generic (	
	vdata : real_vector;  -- v-pulse data 
	tdata : real_vector   -- time-data for v-pulse
	);

port (	
	terminal pos, neg :  electrical
	);
end entity v_pwl_full;


architecture ideal of v_pwl_full is

QUANTITY v across i through pos TO neg;
signal tick : std_logic := '0';                -- Sync signal for tdata "tracking"

begin
				
sync_tdata: process is
variable next_tick_delay : real := 0.0;  -- Time increment to the next time-point in tdata
begin	
	wait until domain = time_domain;
	loop
		next_tick_delay := next_increment(NOW,tdata);
		tick <= (not tick) after (integer(next_tick_delay * 1.0e15) * 1 fs);
		wait on tick;
	end loop;
end process sync_tdata;

break on tick;		-- Forces analog solution point at all tdata time-points
		
v == pwl_dim1_flat(NOW, tdata, vdata);

end architecture ideal;


library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;

library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
use IEEE_proposed.mechanical_systems.all;
use IEEE_proposed.fluidic_systems.all;
use IEEE_proposed.thermal_systems.all;
use IEEE_proposed.radiant_systems.all;

entity tb_CS5_Amp_Lim is
end tb_CS5_Amp_Lim;

architecture TB_CS5_Amp_Lim of tb_CS5_Amp_Lim is
    -- Component declarations
    -- Signal declarations
    terminal amp_in : electrical;
    terminal gear_out : rotational;
    terminal link_in : translational;
    terminal link_out : translational;
    terminal mot_in : electrical;
    terminal mot_out : rotational_v;
    terminal pos_fb_v : electrical;
    terminal power : electrical;
    terminal rudder_in : rotational;
    terminal src_in : electrical;
    terminal XSIG010068 : electrical;
begin
    -- Signal assignments
    -- Component instances
    rudder_servo1 : entity work.rudder_servo
        port map(
            servo_out => amp_in,
            servo_in => src_in,
            pos_fb => pos_fb_v
        );
    gear3 : entity work.gear_rv_r(ideal)
        generic map(
            ratio => 0.01
        )
        port map(
            rotv1 => mot_out,
            rot2 => gear_out
        );
    r2v : entity work.rot2v(bhv)
        generic map(
            k => 1.0
        )
        port map(
            output => pos_fb_v,
            input => gear_out
        );
    r2t : entity work.horn_r2t(bhv)
        port map(
            theta => gear_out,
            pos => link_in
        );
    t2r : entity work.horn_t2r(bhv)
        port map(
            theta => rudder_in,
            pos => link_out
        );
    motor1 : entity work.DC_Motor(basic)
        generic map(
            j => 168.0e-9,
            d => 5.63e-6,
            l => 2.03e-3,
            kt => 3.43e-3,
            r_wind => 2.2
        )
        port map(
            p1 => mot_in,
            p2 => ELECTRICAL_REF,
            shaft_rotv => mot_out
        );
    stop1 : entity work.stop_r(ideal)
        generic map(
            ang_min => -1.05,
            ang_max => 1.05,
            k_stop => 1.0e6,
            damp_stop => 1.0e2
        )
        port map(
            ang1 => gear_out,
            ang2 => ROTATIONAL_REF
        );
    XCMP35 : entity work.tran_linkage(a1)
        port map(
            p2 => link_out,
            p1 => link_in
        );
    XCMP36 : entity work.rudder(bhv)
        generic map(
            k => 0.2
        )
        port map(
            rot => rudder_in
        );
    R2w : entity work.resistor(ideal)
        generic map(
            res => 1000.0
        )
        port map(
            p1 => XSIG010068,
            p2 => ELECTRICAL_REF
        );
    XCMP55 : entity work.amp_lim(simple)
        port map(
            input => amp_in,
            output => mot_in,
            ps => power
        );
    v9 : entity work.v_pulse(ideal)
        generic map(
            initial => 0.0,
            pulse => 4.8,
            ti2p => 300 ms,
            tp2i => 300 ms,
            delay => 100 ms,
            width => 5 ms,
            period => 605 ms
        )
        port map(
            pos => src_in,
            neg => ELECTRICAL_REF
        );
    XCMP57 : entity work.v_pwl_full(ideal)
        generic map(
            tdata => (0.0,100.0e-3,400.0e-3,900.0e-3,1300.0e-3,1800.0e-3,2300.0e-3,2600.0e-3, 2900.0e-3),
            vdata => (0.0,0.0,2.4,2.4,4.7,4.7,1.0,1.0,0.0)
        )
        port map(
            pos => XSIG010068,
            neg => ELECTRICAL_REF
        );
    XCMP60 : entity work.v_pwl_full(ideal)
        generic map(
            vdata => (4.8,4.8,4.4,4.4,4.0,4.0,3.6,3.6,3.2,3.2),
            tdata => (0.0,705.0e-3,706.0e-3,1310.0e-3,1320.0e-3,1915.0e-3,1925.0e-3,2520.0e-3,2530.0e-3,3125.0e-3)
        )
        port map(
            pos => power,
            neg => ELECTRICAL_REF
        );
end TB_CS5_Amp_Lim;
--

