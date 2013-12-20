
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
-- Last update: 2002/05/21
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

-- Copyright Mentor Graphics Corporation 2001
-- Confidential Information Provided Under License Agreement for Internal Use Only

-- Constant Voltage Source (Includes Frequency Domain settings)

LIBRARY IEEE;
USE IEEE.MATH_REAL.ALL;
-- Use proposed IEEE natures and packages
LIBRARY IEEE_proposed;
USE IEEE_proposed.ELECTRICAL_SYSTEMS.ALL;

ENTITY v_constant IS

-- Initialize parameters
  GENERIC (
    level      : VOLTAGE;		-- Constant voltage value (V)
    ac_mag     : VOLTAGE := 1.0;	-- AC magnitude (V)
    ac_phase   : real := 0.0);		-- AC phase (degrees)

-- Define ports as electrical terminals
  PORT (
    TERMINAL pos, neg : ELECTRICAL);

END ENTITY v_constant;

-- Ideal Architecture (I = constant)
ARCHITECTURE ideal OF v_constant IS
  
-- Declare Branch Quantities
  QUANTITY v ACROSS i THROUGH pos TO neg;
-- Declare quantity in frequency domain for AC analysis  
  QUANTITY ac_spec : real SPECTRUM ac_mag, math_2_pi*ac_phase/360.0;

BEGIN

  IF DOMAIN = QUIESCENT_DOMAIN or DOMAIN = TIME_DOMAIN USE
	v == level;
  ELSE	
  	v == ac_spec;  -- used for Frequency (AC) analysis
  END USE;

END ARCHITECTURE ideal;
--

-- C:\Rehan\Cs5\design_definition\hdl\vhdl\switch_dig_log.vhd
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use IEEE.math_real.all;

library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;

entity switch_dig_log is
generic
(
	trans_time : real := 1.0e-9;
	r_closed : resistance := 1.0e-3;
	r_open : resistance := 1.0e6
);
port
(
      terminal p1 : electrical ;
      sw_state : in std_logic ;
      terminal p2 : electrical 
);

begin

end switch_dig_log ;

-----------------------------------------------------------------------------------------
architecture linear of switch_dig_log is
  signal r_sig : resistance := r_open;   -- create internal signal for CreateState process
  quantity v across i through p1 to p2;
  quantity r  : resistance;

begin
  -- purpose: Detect Switch state and assign resistance value to r_sig
  -- type   : combinational
  -- inputs : sw_state
  -- outputs: r_sig
  DetectState: process (sw_state)
  begin  -- process DetectState
    if (sw_state'event and sw_state = '0') then
      r_sig <= r_open;
    elsif (sw_state'event and sw_state = '1') then
      r_sig <= r_closed;
    end if;
  end process DetectState;

-- Characteristic equations
  r == r_sig'ramp(trans_time, trans_time);
  v == r*i;
end architecture linear;

-------------------------------------------------------------------------------------------
architecture log of switch_dig_log is
  constant log10_r_open : real := log10(r_open);
  constant log10_r_closed : real := log10(r_closed);
  signal log10_r_sig : resistance := log10_r_open;   -- create internal signal for CreateState process
  quantity v across i through p1 to p2;
  quantity r : resistance;
  quantity log10_r  : real;

begin
  -- purpose: Detect Switch state and assign resistance value to r_sig
  -- type   : combinational
  -- inputs : sw_state
  -- outputs: r_sig
  DetectState: process (sw_state)
  begin  -- process DetectState
    if (sw_state'event and sw_state = '0') then
      log10_r_sig <= log10_r_open;
    elsif (sw_state'event and sw_state = '1') then
      log10_r_sig <= log10_r_closed;
    end if;
  end process DetectState;

-- Characteristic equations
  log10_r == log10_r_sig'ramp(trans_time, trans_time);
  r == 10**log10_r;
  v == r*i;
end architecture log;
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
-- File       : opamp.vhd
-- Author     : Mentor Graphics
-- Created    : 2001/06/16
-- Last update: 2001/06/16
-------------------------------------------------------------------------------
-- Description: 3-pin OpAmp model with behavioral architecture
--              Uses Q'LTF function to define open-loop response
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version     Author              Description
-- 2001/06/16  1.0         Mentor Graphics     Created    
-------------------------------------------------------------------------------

library IEEE;
use IEEE.math_real.all;

-- Use proposed IEEE natures and packages
library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;

entity opamp is
-- Initialize parameters
  generic (rin   : resistance := 1.0e6;    -- Input resistance [Ohms]
           rout  : resistance := 100.0;    -- Output resistance (Ohms]
           avol  : real       := 100.0e3;  -- Open loop gain
           f_0dB : real       := 1.0e6     -- Unity Gain Frequency [Hz]
           );
-- Define ports as electrical terminals
  port (
    terminal in_pos, in_neg, output : electrical);

end entity opamp;

-------------------------------------------------------------------------------
-- Basic Architecture
-- Characteristics modeled:
--      1. Open loop gain
--      2. Frequency characteristics (single pole response) 
--      3. Input and output resistance
-- Uses Q'Ltf function to create open loop gain and roll off
-------------------------------------------------------------------------------
architecture basic of opamp is
  -- Declare constants
  constant f_3db : real        := f_0db / avol;     -- -3dB frequency
  constant w_3dB : real        := math_2_pi*f_3dB;  -- -3dB freq in radians
  -- Numerator and denominator for Q'LTF function
  constant num   : real_vector := (0 => avol);
  constant den   : real_vector := (1.0, 1.0/w_3dB);
  -- Declare input and output quantities
  quantity v_in across i_in through in_pos to in_neg;
  quantity v_out across i_out through output;

begin  -- ARCHITECTURE basic

  i_in  == v_in / rin;                       -- input current
  v_out == v_in'ltf(num, den) + i_out*rout;  -- output voltage

end architecture basic;

-------------------------------------------------------------------------------
-- Copyright (c) 2001 Mentor Graphics Corporation
-------------------------------------------------------------------------------
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
-- File       : comparator_d.vhd
-- Author     : Mentor Graphics
-- Created    : 2001/08/03
-- Last update: 2001/08/03
-------------------------------------------------------------------------------
-- Description: Voltage comparator with digital output
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version     Author              Description
-- 2001/08/03  1.0         Mentor Graphics     Created    
-------------------------------------------------------------------------------

-- Use IEEE natures and packages
library IEEE;
use ieee.std_logic_1164.all;

-- Use proposed IEEE natures and packages
library IEEE_proposed;
use IEEE_proposed.ELECTRICAL_SYSTEMS.all;
use IEEE_proposed.ENERGY_SYSTEMS.all;

entity comparator_d is

  port (
    terminal in_pos : electrical;
    terminal in_neg : electrical;
    signal output : out std_logic := '1'      -- Digital output
    );

end comparator_d;
-------------------------------------------------------------------------------
-- Behavioral architecture
-------------------------------------------------------------------------------
architecture behavioral of comparator_d is
  quantity Vin across in_pos;
  quantity Vref across in_neg;

begin  -- behavioral

  -- purpose: Detect threshold crossing and assign event on output
  -- type   : combinational
  -- inputs : vin'above(thres)
  -- outputs: pulse_signal  
  process (Vin'above(Vref)) is
  begin  -- PROCESS
    if Vin'above(Vref) then
      output <= '1' after 1us;
    else
      output <= '0' after 1us;
    end if;
  end process;

end behavioral;

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
    ti2p     : time    := 1ns;          -- initial to pulse [Sec]
    tp2i     : time    := 1ns;          -- pulse to initial [Sec]
    delay    : time    := 0ms;          -- delay time [Sec]
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

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;

library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
use IEEE_proposed.mechanical_systems.all;

entity pwm_mac is
    port(
        terminal inp : electrical;
        terminal inm : electrical;
        dig_out : out std_logic
    );
end pwm_mac;

architecture pwm_mac of pwm_mac is
    -- Component declarations
    -- Signal declarations
    terminal cmp_in : electrical;
    terminal plse_in : electrical;
    terminal XSIG010002 : electrical;
    terminal XSIG010003 : electrical;
begin
    -- Signal assignments
    -- Component instances
    U1 : entity work.opamp(basic)
        port map(
            in_neg => XSIG010002,
            in_pos => inm,
            output => cmp_in
        );
    R1 : entity work.resistor(ideal)
        generic map(
            res => 10.0e3
        )
        port map(
            p1 => XSIG010002,
            p2 => cmp_in
        );
    v2 : entity work.v_constant(ideal)
        generic map(
            level => 0.0
        )
        port map(
            pos => XSIG010003,
            neg => ELECTRICAL_REF
        );
    R2 : entity work.resistor(ideal)
        generic map(
            res => 10.0e3
        )
        port map(
            p1 => plse_in,
            p2 => XSIG010002
        );
    R3 : entity work.resistor(ideal)
        generic map(
            res => 10.0e3
        )
        port map(
            p1 => inp,
            p2 => XSIG010002
        );
    XCMP4 : entity work.comparator_d(behavioral)
        port map(
            output => dig_out,
            in_pos => XSIG010003,
            in_neg => cmp_in
        );
    v9 : entity work.v_pulse(ideal)
        generic map(
            initial => -4.7,
            pulse => 4.7,
            ti2p => 200 us,
            tp2i => 200 us,
            delay => 1 us,
            width => 1 us,
            period => 405 us
        )
        port map(
            pos => plse_in,
            neg => ELECTRICAL_REF
        );
end pwm_mac;
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
-- File       : prop_pwl.vhd
-- Author     : Mentor Graphics
-- Created    : 2001/06/16
-- Last update: 2001/06/16
-------------------------------------------------------------------------------
-- Description: Propeller Load (Rotational_V domain)
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version     Author              Description
-- 2001/06/16  1.0         Mentor Graphics     Created    
-------------------------------------------------------------------------------

library ieee;
use ieee.math_real.all;
package pwl_functions is
function pwl_dim1_extrap (x : in real; xdata, ydata : in real_vector ) 
	return real;
function interpolate (x,y2,y1,x2,x1 : in real) 
	return real;
function extrapolate (x,y2,y1,x2,x1 : in real) 
	return real;
end package pwl_functions;

package body pwl_functions is
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
	
	 function extrapolate (x,y2,y1,x2,x1 : in real) 
		return real is 
		variable m, yvalue : real;
	begin
		assert (x1 /= x2)
			report "extrapolate: x1 cannot be equal to x2"
			severity error;
		assert (x <= x1) or (x >= x2) 
			report "extrapolate: x is within x1, x2 bounds; interpolation will be performed"
			severity warning;
	
		m := (y2 - y1)/(x2 - x1);
		yvalue := y1 + m*(x - x1);
		return yvalue;
	end function extrapolate;

--  Created a new pwl_dim1_extrap function that returns extrapolated yvalue for "out-of-range" x value.

	function pwl_dim1_extrap (x : in real; xdata, ydata : in real_vector ) 
		return real is 
		variable xvalue, yvalue, m : real;
                variable start, fin, mid: integer; 
	begin
		if x <= xdata(0) then
			yvalue := extrapolate(x,ydata(1),ydata(0),xdata(1),xdata(0));
			return yvalue;
		end if;
		if x >= xdata(xdata'right) then
			yvalue := extrapolate(x,ydata(ydata'right),ydata(ydata'right-1),xdata(xdata'right),xdata(xdata'right-1));
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
	end function pwl_dim1_extrap;
end package body pwl_functions;

library IEEE_proposed; use IEEE_proposed.mechanical_systems.all;
library ieee; use ieee.math_real.all;
use work.pwl_functions.all;

entity prop_pwl is
generic (	
	ydata : real_vector;  -- torque data 
	xdata : real_vector   -- velocity data
	);
  port (terminal shaft1 : rotational_v);
end entity prop_pwl;

architecture ideal of prop_pwl is
	quantity w across torq through shaft1 to rotational_v_ref;
begin				
	torq == pwl_dim1_extrap(w, xdata, ydata);
end architecture ideal;
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
-- File       : diode_pwl.vhd
-- Author     : Mentor Graphics
-- Created    : 2001/06/16
-- Last update: 2001/06/16
-------------------------------------------------------------------------------
-- Description: Diode model with ideal architecture
--              Currently no Generics due to bug in DV 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version     Author              Description
-- 2001/06/16  1.0         Mentor Graphics     Created    
-------------------------------------------------------------------------------

library IEEE;
use IEEE.math_real.all;

-- Use proposed IEEE natures and packages
library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
-- energy_systems package needed for Boltzmann constant (K = Joules/Kelvin)
use IEEE_proposed.energy_systems.all;

ENTITY diode_pwl IS
	GENERIC (
		ron : real;	-- equivalent series resistance
		roff : real);	-- leakage resistance
	PORT (
		TERMINAL 	p,	-- positive pin
				m : electrical); -- minus pin
END ENTITY diode_pwl;

ARCHITECTURE simple OF diode_pwl IS
	QUANTITY v across i through p TO m;

BEGIN			-- simple ARCHITECTURE
	if v'Above(0.0) use
		i == v/ron;
	elsif not v'Above(0.0) use
		i == v/roff;
	else
		i == 0.0;
	end use;
	break on v'Above(0.0);
END ARCHITECTURE simple;

-- Copyright Mentor Graphics Corporation 2001
-- Confidential Information Provided Under License Agreement for Internal Use Only

-- Electrical sinusoidal voltage source (v_sine.vhd)

LIBRARY IEEE;
USE IEEE.MATH_REAL.ALL;
-- Use proposed IEEE natures and packages
LIBRARY IEEE_proposed;
USE IEEE_proposed.ELECTRICAL_SYSTEMS.ALL;


ENTITY v_sine IS
  
-- Initialize parameters
  GENERIC (
    freq      : real;                     -- frequency,       [Hertz]
    amplitude : real;                     -- amplitude,       [Volt]
    phase     : real := 0.0;              -- initial phase,   [Degree]
    offset    : real := 0.0;              -- DC value,        [Volt]
    df        : real := 0.0;              -- damping factor,  [1/second]
    ac_mag    : real := 1.0;              -- AC magnitude,    [Volt]
    ac_phase  : real := 0.0);             -- AC phase,        [Degree]

-- Define ports as electrical terminals
  PORT (
    TERMINAL pos, neg : ELECTRICAL);

END ENTITY v_sine;

-- Ideal Architecture
ARCHITECTURE ideal OF v_sine IS
-- Declare Branch Quantities
  QUANTITY v ACROSS i THROUGH pos TO neg;
-- Declare Quantity for Phase in radians (calculated below)
  QUANTITY phase_rad : real;          
-- Declare Quantity in frequency domain for AC analysis
  QUANTITY ac_spec : real SPECTRUM ac_mag, math_2_pi*ac_phase/360.0;

BEGIN  
-- Convert phase to radians
  phase_rad == math_2_pi *(freq * NOW + phase / 360.0);
  
  IF DOMAIN = QUIESCENT_DOMAIN OR DOMAIN = TIME_DOMAIN USE
    v == offset + amplitude * sin(phase_rad) * EXP(-NOW * df);
  ELSE 
    v == ac_spec;    -- used for Frequency (AC) analysis
  END USE;

END ARCHITECTURE ideal;

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;

library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
use IEEE_proposed.mechanical_systems.all;

entity tb_CS5_Prop is
end tb_CS5_Prop;

architecture TB_CS5_Prop of tb_CS5_Prop is
    -- Component declarations
    -- Signal declarations
    terminal prop : rotational_v;
    terminal prop_amp_in : electrical;
    terminal prop_mtr_in : electrical;
    terminal prop_pwr : electrical;
    signal pwm_out : std_logic;
begin
    -- Signal assignments
    -- Component instances
    motor2 : entity work.DC_Motor(basic)
        generic map(
            kt => 30.1e-3,
            l => 40.0e-6,
            d => 5.63e-12,
            j => 315.0e-6,
            r_wind => 0.16
        )
        port map(
            p1 => prop_mtr_in,
            p2 => ELECTRICAL_REF,
            shaft_rotv => prop
        );
    v4 : entity work.v_constant(ideal)
        generic map(
            level => 42.0
        )
        port map(
            pos => prop_pwr,
            neg => ELECTRICAL_REF
        );
    sw2 : entity work.switch_dig_log
        port map(
            sw_state => pwm_out,
            p2 => prop_mtr_in,
            p1 => prop_pwr
        );
    pwm1 : entity work.pwm_mac
        port map(
            inp => prop_amp_in,
            dig_out => pwm_out,
            inm => ELECTRICAL_REF
        );
    XCMP37 : entity work.prop_pwl(ideal)
        generic map(
            ydata => (0.233, 0.2865, 0.347, 0.4138, 0.485, 0.563, 0.645, 0.735, 0.830, 0.93, 1.08),
            xdata => (471.2, 523.6, 576.0, 628.3, 680.7, 733.0, 785.4, 837.7, 890.0, 942.5, 994.8)
        )
        port map(
            shaft1 => prop
        );
    D4 : entity work.diode_pwl(simple)
        generic map(
            ron => 0.001,
            roff => 100.0e3
        )
        port map(
            p => ELECTRICAL_REF,
            m => prop_mtr_in
        );
    v8 : entity work.v_sine(ideal)
        generic map(
            freq => 1.0,
            amplitude => 2.3,
            phase => 0.0,
            offset => 2.3
        )
        port map(
            pos => prop_amp_in,
            neg => ELECTRICAL_REF
        );
end TB_CS5_Prop;
--










