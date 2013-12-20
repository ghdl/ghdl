
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
use IEEE.MATH_REAL.all;
library IEEE_proposed;
use IEEE_proposed.ELECTRICAL_SYSTEMS.all;

entity v_sine is

  generic (
    freq      : real;                   -- frequency [Hertz]
    amplitude : voltage;                -- amplitude [Volts]
    phase     : real    := 0.0;         -- initial phase [Degrees]
    offset    : voltage := 0.0;         -- DC value [Volts]
    df        : real    := 0.0;         -- damping factor [1/second]
    ac_mag    : voltage := 1.0;         -- AC magnitude [Volts]
    ac_phase  : real    := 0.0);        -- AC phase [Degrees]

  port (
    terminal pos, neg : electrical);

end entity v_sine;

-------------------------------------------------------------------------------
-- Ideal Architecture
-------------------------------------------------------------------------------
architecture ideal of v_sine is
-- Declare Branch Quantities
  quantity v across i through pos to neg;
-- Declare Quantity for Phase in radians (calculated below)
  quantity phase_rad : real;
-- Declare Quantity in frequency domain for AC analysis
  quantity ac_spec   : real spectrum ac_mag, math_2_pi*ac_phase/360.0;

begin
-- Convert phase to radians
  phase_rad == math_2_pi *(freq * NOW + phase / 360.0);

  if domain = quiescent_domain or domain = time_domain use
    v == offset + amplitude * sin(phase_rad) * EXP(-NOW * df);
  else
    v == ac_spec;           -- used for Frequency (AC) analysis
  end use;

end architecture ideal;

-------------------------------------------------------------------------------
-- Copyright (c) 2001 Mentor Graphics Corporation
-------------------------------------------------------------------------------
library IEEE;
library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
use IEEE_proposed.mechanical_systems.all;
use IEEE_proposed.fluidic_systems.all;
use IEEE_proposed.thermal_systems.all;
use IEEE_proposed.radiant_systems.all;

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
library IEEE; use ieee.math_real.all;

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

-- Use IEEE_proposed instead of disciplines
library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
library IEEE;
use ieee.math_real.all;

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
-- Control Horn for Rudder Control
--
--  Transfer Function:
--                            
--  pos_t_out =   R*sin(theta) 
--
-- Where pos_t = output translational position,
-- R = horn radius, 
-- theta_in = input rotational angle
-------------------------------------------------------------------------------

-- Use IEEE_proposed instead of disciplines
library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
library IEEE;
use ieee.math_real.all;

entity ctl_horn_e is
  
  generic (
    R   : real := 1.0);            -- horn radius

  port (
    terminal theta_in    : electrical;   -- input port
    terminal pos_t_out   : electrical);  -- output port
     
end entity ctl_horn_e;

architecture bhv of ctl_horn_e is
  quantity vin across theta_in to electrical_ref;
  quantity vout across iout through pos_t_out to electrical_ref;

  begin  -- bhv
   vout == R*sin(vin);
end bhv;
--
-------------------------------------------------------------------------------
-- Rudder Model
--
--  Transfer Function:
--                            
--  theta_out =   arcsin(pos_t_in/R) 
--
-- Where pos_t_in = input translational position,
-- R = horn radius, 
-- theta_out = output rotational angle
-------------------------------------------------------------------------------

-- Use IEEE_proposed instead of disciplines
library IEEE;
use ieee.math_real.all;
library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;


entity rudder_horn_e is
  
  generic (
    R   : real := 1.0);            -- Rudder horn radius

  port (
    terminal pos_t_in    : electrical;   -- input port
    terminal theta_out   : electrical);  -- output port
     
end entity rudder_horn_e;

architecture bhv of rudder_horn_e is
  quantity vin across pos_t_in to electrical_ref;
  quantity vout across iout through theta_out to electrical_ref;

  begin  -- bhv
   vout == arcsin(vin/R);
end bhv;
--
-------------------------------------------------------------------------------
-- Integrator
--
--  Transfer Function:
--
--               k         
--  H(s) =   ---------
--               s 
--
-------------------------------------------------------------------------------

-- Use IEEE_proposed instead of disciplines
library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
library IEEE;
use ieee.math_real.all;

entity integ_1_e is
	generic (
		k: real := 1.0;		-- Gain
		init: real := 0.0);  -- Initial value of output
	port 	(terminal input: electrical;
		terminal output: electrical);
end entity integ_1_e;

architecture simple of integ_1_e is
  QUANTITY vin ACROSS input TO ELECTRICAL_REF;
  QUANTITY vout ACROSS iout THROUGH output TO ELECTRICAL_REF;

  quantity vin_temp : real;
begin
  vin_temp == vin;
  IF domain = QUIESCENT_DOMAIN AND init /= 0.0 USE
  	vout == init;
  ELSE 
	vout == k*vin_temp'INTEG;

  END USE;

end architecture simple;
--
-------------------------------------------------------------------------------
-- Second Order Lowpass filter
--
--  Transfer Function:
--
--                    w1*w2         
--   H(s) =  k * ----------------
--               (s + w1)(s + w2)
--
-- DC Gain = k
-------------------------------------------------------------------------------

-- Use IEEE_proposed instead of disciplines
library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
library IEEE; use ieee.math_real.all;

entity lpf_1_e is
  generic (
    fp   : real;            -- pole freq
    gain : real := 1.0);    -- filter gain

    port 	(	terminal input: electrical;
			terminal output: electrical);
end entity lpf_1_e;

architecture simple of lpf_1_e is
  QUANTITY vin ACROSS input TO ELECTRICAL_REF;
  QUANTITY vout ACROSS iout THROUGH output TO ELECTRICAL_REF;

  constant wp : real := math_2_pi*fp;    
  constant num : real_vector := (0 => wp*gain);  -- 0=> is needed to give
                                                   -- index when only a single
                                                   -- element is used.
  constant den : real_vector := (wp, 1.0);
  quantity vin_temp : real;  

begin
	vin_temp == vin;   -- intermediate variable (vin) req'd for now
	vout == vin_temp'ltf(num, den);
end architecture simple;

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;

library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
use IEEE_proposed.mechanical_systems.all;

entity TB_CS2_S_Domain is
end TB_CS2_S_Domain;

architecture TB_CS2_S_Domain of TB_CS2_S_Domain is
    -- Component declarations
    -- Signal declarations
    terminal comp_in : electrical;
    terminal ctl_horn_out : electrical;
    terminal err_limit_in : electrical;
    terminal error : electrical;
    terminal gear_out : electrical;
    terminal integ_out : electrical;
    terminal load_trq : electrical;
    terminal mtr_fb : electrical;
    terminal mtr_gen_trq : electrical;
    terminal mtr_in : electrical;
    terminal mtr_out : electrical;
    terminal pos_fb : electrical;
    terminal rudder : electrical;
    terminal rudder_in : electrical;
    terminal src_in : electrical;
    terminal XSIG010043 : electrical;
    terminal XSIG010044 : electrical;
    terminal XSIG010046 : electrical;
    terminal XSIG010050 : electrical;
begin
    -- Signal assignments
    -- Component instances
    v_source : entity work.v_sine(ideal)
        generic map(
            amplitude => 4.8,
            freq => 1.0
        )
        port map(
            pos => src_in,
            neg => ELECTRICAL_REF
        );
    sum_pos : entity work.sum2_e(simple)
        port map(
            in1 => src_in,
            in2 => pos_fb,
            output => error
        );
    loop_comp : entity work.lead_lag_e(simple)
        generic map(
            f1 => 5.0,
            k => 4000.0,
            f2 => 20000.0
        )
        port map(
            input => comp_in,
            output => err_limit_in
        );
    pos_fb_gain : entity work.gain_e(simple)
        generic map(
            k => -4.57
        )
        port map(
            input => rudder_in,
            output => pos_fb
        );
    mech_limit : entity work.limiter_2_e(simple)
       generic map(
            limit_high => 1.05,
            limit_low => -1.05
        )
        port map(
            input => integ_out,
            output => rudder_in
        );
    gear_box_horn : entity work.ctl_horn_e(bhv)
        port map(
            theta_in => rudder_in,
            pos_t_out => ctl_horn_out
        );
    rudder_horn : entity work.rudder_horn_e(bhv)
        port map(
            pos_t_in => ctl_horn_out,
            theta_out => rudder
        );
    mtr_Kt : entity work.gain_e(simple)
        generic map(
            k => 3.43e-3
        )
        port map(
            input => XSIG010044,
            output => mtr_gen_trq
        );
    gear_box : entity work.gain_e(simple)
        generic map(
            k => 0.01
        )
        port map(
            input => mtr_out,
            output => gear_out
        );
    mtr_Ke : entity work.gain_e(simple)
        generic map(
            k => -3.43e-3
        )
        port map(
            input => mtr_out,
            output => mtr_fb
        );
    sum_mtr_in : entity work.sum2_e(simple)
        port map(
            in1 => mtr_in,
            in2 => mtr_fb,
            output => XSIG010043
        );
    sum_load_trq : entity work.sum2_e(simple)
        port map(
            in1 => mtr_gen_trq,
            in2 => load_trq,
            output => XSIG010046
        );
    integrator : entity work.integ_1_e(simple)
        generic map(
            k => 1.0
        )
        port map(
            input => gear_out,
            output => integ_out
        );
    rudder_trq : entity work.gain_e(simple)
        generic map(
            k => -0.2
        )
        port map(
            input => XSIG010050,
            output => load_trq
        );
    trq_fb_gain : entity work.gain_e(simple)
        generic map(
            k => 0.01
        )
        port map(
            input => rudder_in,
            output => XSIG010050
        );
    mtr_elec_pole : entity work.lpf_1_e(simple)
        generic map(
            gain => 0.4545,
            fp => 172.48
        )
        port map(
            input => XSIG010043,
            output => XSIG010044
        );
    mtr_mech_pole : entity work.lpf_1_e(simple)
        generic map(
            gain => 177.67e3,
            fp => 5.33
        )
        port map(
            input => XSIG010046,
            output => mtr_out
        );
    loop_gain : entity work.gain_e(simple)
        generic map(
            k => 100.0
        )
        port map(
            input => error,
            output => comp_in
        );
    err_limit : entity work.limiter_2_e(simple)
       generic map(
            limit_high => 4.8,
            limit_low => -4.8
        )
        port map(
            input => err_limit_in,
            output => mtr_in
        );
end TB_CS2_S_Domain;


