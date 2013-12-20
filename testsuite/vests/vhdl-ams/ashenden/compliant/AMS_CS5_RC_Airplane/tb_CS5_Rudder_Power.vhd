
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
    servo_limiter : entity work.limiter_2_e(simple)
      generic map(
            limit_high => 4.8,
            limit_low => -4.8
        )
        port map(
            input => ll_out,
            output => servo_out
        );
    lead_lag : entity work.lead_lag_e(simple)
        generic map(
            k => 400.0,
            f1 => 5.0,
            f2 => 2000.0
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
-- Last update: 2002/05/21
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

------------------------------------------------------------------------------
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
-- File       : buff.vhd
-- Author     : Mentor Graphics
-- Created    : 2001/06/16
-- Last update: 2001/06/16
-------------------------------------------------------------------------------
-- Description: Simple Buffer with delay time
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version     Author              Description
-- 2001/06/16  1.0         Mentor Graphics     Created    
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

entity buff is
  generic (
    delay : time := 0 ns);              -- Delay time

  port (
    input  : in  std_logic;
    output : out std_logic);

end entity buff;

architecture ideal of buff is
  
begin
  output <= input after delay;
  
end architecture ideal;


-------------------------------------------------------------------------------
-- Copyright (c) 2001 Mentor Graphics Corporation
-------------------------------------------------------------------------------
-- Copyright Mentor Graphics Corporation 2001
-- Confidential Information Provided Under License Agreement for Internal Use Only

-- Inverter
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

ENTITY inverter IS
  GENERIC (
    delay : time := 0 ns);              -- Delay time
  
  PORT (
    input : IN  std_logic;
    output : OUT std_logic);
  
END ENTITY inverter;

ARCHITECTURE ideal OF inverter IS
BEGIN
   output <= NOT input AFTER delay;
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

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;

library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
use IEEE_proposed.mechanical_systems.all;

entity pwm_H_bridge is
    port(
        terminal mot_ccw : electrical;
        terminal pwr_in : electrical;
        terminal mot_cw : electrical;
        terminal src_in : electrical
    );
end pwm_H_bridge;

architecture pwm_H_bridge of pwm_H_bridge is
    -- Component declarations
    -- Signal declarations
    signal pwm_out : std_logic;
    signal sw_ccw : std_logic;
    signal sw_cw : std_logic;
begin
    -- Signal assignments
    -- Component instances
    sw2 : entity work.switch_dig_log(linear)
        generic map(
            trans_time => 1.0e-5,
            r_closed => 0.1
        )
        port map(
            sw_state => sw_cw,
            p2 => pwr_in,
            p1 => mot_cw
        );
    sw3 : entity work.switch_dig_log(linear)
        generic map(
            trans_time => 1.0e-5,
            r_closed => 0.1
        )
        port map(
            sw_state => sw_ccw,
            p2 => mot_cw,
            p1 => ELECTRICAL_REF
        );
    U1 : entity work.buff(ideal)
        port map(
            input => pwm_out,
            output => sw_cw
        );
    U2 : entity work.inverter(ideal)
        port map(
            input => pwm_out,
            output => sw_ccw
        );
    sw5 : entity work.switch_dig_log(linear)
        generic map(
            trans_time => 1.0e-5,
            r_closed => 0.1
        )
        port map(
            sw_state => sw_ccw,
            p2 => pwr_in,
            p1 => mot_ccw
        );
    sw6 : entity work.switch_dig_log(linear)
        generic map(
            trans_time => 1.0e-5,
            r_closed => 0.1
        )
        port map(
            sw_state => sw_cw,
            p2 => mot_ccw,
            p1 => ELECTRICAL_REF
        );
    pwm : entity work.pwm_mac
        port map(
            inp => src_in,
            dig_out => pwm_out,
            inm => ELECTRICAL_REF
        );
    D7 : entity work.diode_pwl(simple)
        generic map(
            roff => 100.0e3,
            ron => 0.001
        )
        port map(
            p => mot_cw,
            m => pwr_in
        );
    D8 : entity work.diode_pwl(simple)
        generic map(
            ron => 0.001,
            roff => 100.0e3
        )
        port map(
            p => mot_ccw,
            m => pwr_in
        );
    D9 : entity work.diode_pwl(simple)
        generic map(
            ron => 0.001,
            roff => 100.0e3
        )
        port map(
            p => ELECTRICAL_REF,
            m => mot_cw
        );
    D10 : entity work.diode_pwl(simple)
        generic map(
            ron => 0.001,
            roff => 100.0e3
        )
        port map(
            p => ELECTRICAL_REF,
            m => mot_ccw
        );
end pwm_H_bridge;
--
-- Copyright Mentor Graphics Corporation 2001
-- Confidential Information Provided Under License Agreement for Internal Use Only

-- Electrical sinusoidal voltage source (stick.vhd)

LIBRARY IEEE;
USE IEEE.MATH_REAL.ALL;
-- Use proposed IEEE natures and packages
LIBRARY IEEE_proposed;
USE IEEE_proposed.ELECTRICAL_SYSTEMS.ALL;


ENTITY stick IS
  
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
    TERMINAL v_out : ELECTRICAL);

END ENTITY stick;

-- Ideal Architecture
ARCHITECTURE ideal OF stick IS
-- Declare Branch Quantities
  QUANTITY v ACROSS i THROUGH v_out TO electrical_ref;
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
-- File       : inductor.vhd
-- Author     : Mentor Graphics
-- Created    : 2001/06/16
-- Last update: 2001/06/16
-------------------------------------------------------------------------------
-- Description: Electrical Inductor
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version     Author              Description
-- 2001/06/16  1.0         Mentor Graphics     Created    
-------------------------------------------------------------------------------

-- Use proposed IEEE natures and packages
library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;

entity inductor is

  generic (
    ind  : inductance;             -- Nominal inductance
    i_ic : real := real'low);      -- Initial current (use IF statement below
                                   -- to activate) 
                                  
  port (
    terminal p1, p2 : electrical);

end entity inductor;

-------------------------------------------------------------------------------
-- Ideal Architecture (V = L * di/dt)
-- Includes initial condition
-------------------------------------------------------------------------------
architecture ideal of inductor is

-- Declare Branch Quantities
  quantity v across i through p1 to p2;

begin

  if domain = quiescent_domain and i_ic /= real'low use
    i == i_ic;
  else
    v   == ind * i'dot;                   -- characteristic equation
  end use;

end architecture ideal;

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
-- File       : capacitor.vhd
-- Author     : Mentor Graphics
-- Created    : 2001/06/16
-- Last update: 2001/06/16
-------------------------------------------------------------------------------
-- Description: Electrical Capacitor 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version     Author              Description
-- 2001/06/16  1.0         Mentor Graphics     Created    
-------------------------------------------------------------------------------

-- Use proposed IEEE natures and packages
library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;

entity capacitor is

  generic (
    cap             : capacitance;        -- Capacitance [F]
    v_ic            : real := real'low;   -- Initial voltage (activated by
                                          -- IF statement below)
    r_esr            : resistance := 0.0); -- Equivalent Series Capicitance 
	                                      -- (used only in ESR architecture)
  
  port (
    terminal p1, p2 : electrical);

end entity capacitor;

-------------------------------------------------------------------------------
-- Ideal Architecture  (I = C * dV/dt)
-- Includes initial condition
-------------------------------------------------------------------------------
architecture ideal of capacitor is
  
  quantity v across i through p1 to p2;

begin

  if domain = quiescent_domain and v_ic /= real'low use
    v == v_ic;
  else
    i   == cap * v'dot;                  -- characteristic equation
  end use;

end architecture ideal;

-------------------------------------------------------------------------------
-- Architecture includes effects of Equivalent Series Capacitance
-------------------------------------------------------------------------------
architecture ESR of capacitor is
  quantity v across i through p1 to p2;
  quantity vc   : voltage;              -- Internal voltage across capacitor
begin
  if domain = quiescent_domain and v_ic /= real'low use
    vc == v_ic;
	i == 0.0;
  else
    vc == v - (i * r_esr);
    i == cap * vc'dot;

  end use;
end architecture ESR;

-------------------------------------------------------------------------------
-- Copyright (c) 2001 Mentor Graphics Corporation
-------------------------------------------------------------------------------
--
library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;


entity buck_sw is
  
  generic (
	Vd   : voltage := 0.7;             -- Diode Voltage
	Vramp : voltage := 2.5);           -- P-P amplitude of ramp voltage

  port (
    terminal input, output, ref, ctrl: electrical);
  
end entity buck_sw;

architecture average of buck_sw is

  quantity Vout across Iout through output to ref;
  quantity Vin across input to ref;
  quantity Vctrl across ctrl to ref;
  
begin  -- bhv

   Vout + Vd == Vctrl * Vin / Vramp; 

end average;

--

-- Loop control switch
library IEEE;
use IEEE.std_logic_1164.all;

-- Use proposed IEEE natures and packages
library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;

entity sw_LoopCtrl is
  generic (r_open     : resistance := 1.0e6;
           r_closed   : resistance := 1.0e-3;
	   sw_state   : integer    := 1);

  port (terminal c, p1, p2 : electrical);
end entity sw_LoopCtrl;

architecture ideal of sw_LoopCtrl is 
  quantity v1 across i1 through c to p1;
  quantity v2 across i2 through c to p2;
  quantity r1, r2 : resistance;  
begin
    if (sw_state = 1) use
      r1 == r_closed;
      r2 == r_open;
    elsif (sw_state = 2) use
      r1 == r_open;
      r2 == r_closed;
    else
      r1 == r_closed;
      r2 == r_open; 
    end use;

  v1 == r1*i1;
  v2 == r2*i2;
end architecture ideal;
--

library ieee, ieee_proposed;
use ieee.math_real.all;
use IEEE_proposed.electrical_systems.all;

entity comp_2p2z is
  generic (
    gain : real := 100.0;    -- High DC gain for good load regulation       
    fp1  : real := 7.5e3;     -- Pole location to achieve crossover frequency
    fp2  : real := 531.0e3;   -- Pole location to cancel effect of ESR 
    fz1  : real := 806.0;     -- Zero locations to cancel LC filter poles
    fz2  : real := 806.0);
  port (
    terminal input, output, ref : electrical);
end entity comp_2p2z;

architecture ltf of comp_2p2z is
  quantity vin across input to ref;
  quantity vout across iout through output to ref;
  constant wp1 : real        := math_2_pi*fp1;  -- Pole freq (in radians)
  constant wp2 : real        := math_2_pi*fp2;
  constant wz1 : real        := math_2_pi*fz1;  -- Zero freq (in radians)
  constant wz2 : real        := math_2_pi*fz2;
  constant num : real_vector := (1.0, 1.0/wz1 + 1.0/wz2, 1.0/(wz1*wz2));
  constant den : real_vector := (1.0e-9,1.0,1.0/wp1+1.0/wp2,1.0/(wp1*wp2));

begin
     vout == -1.0*gain*vin'ltf(num, den);
end architecture ltf;

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;

library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
use IEEE_proposed.mechanical_systems.all;

entity ex_buck is
    port(
        terminal pwr_out : electrical
    );
end ex_buck;

architecture ex_buck of ex_buck is
    -- Component declarations
    -- Signal declarations
    terminal vcomp_out : electrical;
    terminal vctrl : electrical;
    terminal vctrl_init : electrical;
    terminal vin : electrical;
    terminal vmid : electrical;
    terminal XSIG010004 : electrical;
begin
    -- Signal assignments
    -- Component instances
    l1 : entity work.inductor(ideal)
        generic map(
            ind => 6.5e-3
        )
        port map(
            p1 => vmid,
            p2 => pwr_out
        );
    c1 : entity work.capacitor(ideal)
        generic map(
            cap => 6.0e-6,
            r_esr => 50.0e-3
        )
        port map(
            p1 => pwr_out,
            p2 => ELECTRICAL_REF
        );
    buck_sw1 : entity work.buck_sw(average)
        port map(
            output => vmid,
            ref => ELECTRICAL_REF,
            ctrl => vctrl,
            input => vin
        );
    sw1 : entity work.sw_LoopCtrl(ideal)
        generic map(
            sw_state => 1
        )
        port map(
            p2 => vctrl_init,
            c => vctrl,
            p1 => vcomp_out
        );
    comp_2p2z1 : entity work.comp_2p2z(ltf)
        port map(
            ref => XSIG010004,
            output => vcomp_out,
            input => pwr_out
        );
    v1 : entity work.v_pulse(ideal)
        generic map(
            initial => 42.0,
            pulse => 42.0,
            delay => 10ms,
            width => 100ms,
            period => 1000ms
        )
        port map(
            pos => vin,
            neg => ELECTRICAL_REF
        );
    v2 : entity work.v_constant(ideal)
        generic map(
            level => 0.327
        )
        port map(
            pos => vctrl_init,
            neg => ELECTRICAL_REF
        );
    v3 : entity work.v_constant(ideal)
        generic map(
            level => 4.8
        )
        port map(
            pos => XSIG010004,
            neg => ELECTRICAL_REF
        );
end ex_buck;
--

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;

library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
use IEEE_proposed.mechanical_systems.all;

entity tb_CS5_Rudder_Power is
end tb_CS5_Rudder_Power ;

architecture TB_CS5_Rudder_Power of tb_CS5_Rudder_Power is
    -- Component declarations
    -- Signal declarations
    terminal buck_out : electrical;
    terminal gear_out : rotational;
    terminal link_in : translational;
    terminal link_out : translational;
    terminal mot_ccw : electrical;
    terminal mot_cw : electrical;
    terminal mot_out : rotational_v;
    terminal pos_fb_v : electrical;
    terminal pwm_in : electrical;
    terminal rudder : rotational;
    terminal src_in : electrical;
begin
    -- Signal assignments
    -- Component instances
    rudder_servo1 : entity work.rudder_servo
        port map(
            servo_out => pwm_in,
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
            theta => rudder,
            pos => link_out
        );
    motor1 : entity work.DC_Motor(basic)
        generic map(
            r_wind => 2.2,
            kt => 3.43e-3,
            l => 2.03e-3,
            d => 5.63e-6,
            j => 168.0e-9
        )
        port map(
            p1 => mot_cw,
            p2 => mot_ccw,
            shaft_rotv => mot_out
        );
    stop1 : entity work.stop_r(ideal)
        generic map(
            damp_stop => 1.0e2,
            k_stop => 1.0e6,
            ang_max => 1.05,
            ang_min => -1.05
        )
        port map(
            ang1 => gear_out,
            ang2 => ROTATIONAL_REF
        );
    \linkage\ : entity work.tran_linkage(a1)
        port map(
            p2 => link_out,
            p1 => link_in
        );
    rudder_1 : entity work.rudder(bhv)
        generic map(
            k => 0.02
        )
        port map(
            rot => rudder
        );
    pwm_H_bridge1 : entity work.pwm_H_bridge
        port map(
            src_in => pwm_in,
            mot_cw => mot_cw,
            pwr_in => buck_out,
            mot_ccw => mot_ccw
        );
    XCMP65 : entity work.stick(ideal)
        generic map(
            freq => 1.0,
            amplitude => 4.7,
            phase => 0.0,
            offset => 0.0
        )
        port map(
            v_out => src_in
        );
    ex_buck4 : entity work.ex_buck
        port map(
            pwr_out => buck_out
        );
end TB_CS5_Rudder_Power;
--

