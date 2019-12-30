
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
-- Last update: 2019-12-30
-------------------------------------------------------------------------------
-- Description: Gear Model (ROTATIONAL_V/ROTATIONAL domains)
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version     Author              Description
-- 2001/10/10  1.0         Mentor Graphics     Created    
-------------------------------------------------------------------------------

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

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
use IEEE_proposed.mechanical_systems.all;

entity RF_xmtr_rcvr is
generic (td : time := 0 ns);
port
(
      tdm_in : in std_logic ;
      tdm_out : out std_logic 
);

end RF_xmtr_rcvr;

architecture behavioral of RF_xmtr_rcvr is
begin

tdm_out <= tdm_in after td;

end;
--

-- Copyright Mentor Graphics Corporation 2001
-- Confidential Information Provided Under License Agreement for Internal Use Only

-- Simple Digital-Controlled Two-position Switch Model
-- Switch position 1 ('0') or switch position 2 ('1')

LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;
use IEEE.std_logic_arith.all;
use IEEE.math_real.all;

-- Use proposed IEEE natures and packages
LIBRARY IEEE_proposed;
USE IEEE_proposed.electrical_systems.ALL;

ENTITY switch_dig_2in is
	GENERIC (r_open : RESISTANCE := 1.0e6; -- Open switch resistance
		 	r_closed : RESISTANCE := 0.001; -- Closed switch resistance
		 	trans_time : real := 0.00001); -- Transition time to each position

	PORT (sw_state : in std_logic; -- Digital control input 
    	      TERMINAL p_in1, p_in2, p_out : ELECTRICAL); -- Analog output

END ENTITY switch_dig_2in;


ARCHITECTURE ideal OF switch_dig_2in IS

--	CONSTANT log_r_open : real := log10(r_open);
--	CONSTANT log_r_closed : real := log10(r_closed);
--	SIGNAL r_sig1 : RESISTANCE := log_r_closed; -- Variable to accept switch resistance
--	SIGNAL r_sig2 : RESISTANCE := log_r_open; -- Variable to accept switch resistance
	SIGNAL r_sig1 : RESISTANCE := r_closed; -- Variable to accept switch resistance
	SIGNAL r_sig2 : RESISTANCE := r_open; -- Variable to accept switch resistance
	QUANTITY v1 ACROSS i1 THROUGH p_in1 TO p_out; -- V & I for in1 to out
	QUANTITY v2 ACROSS i2 THROUGH p_in2 TO p_out; -- V & I for in2 to out
	QUANTITY r1 : RESISTANCE; -- Time-varying resistance for in1 to out
	QUANTITY r2 : RESISTANCE; -- Time-varying resistance for in2 to out

BEGIN
 
 PROCESS (sw_state) -- Sensitivity to digital control input
    BEGIN
	  IF (sw_state'event AND sw_state = '0') THEN -- Close sig1, open sig2
	    r_sig1 <= r_closed;
	    r_sig2 <= r_open;
	  ELSIF (sw_state'event AND sw_state = '1') THEN -- Open sig1, close sig2
	    r_sig1 <= r_open;
	    r_sig2 <= r_closed;
	  END IF;
    END PROCESS;

	r1 == r_sig1'ramp(trans_time, trans_time); -- Ensure resistance continuity
	r2 == r_sig2'ramp(trans_time, trans_time); -- Ensure resistance continuity
	v1 == r1*i1; -- Apply Ohm's law to in1
	v2 == r2*i2; -- Apply Ohm's law to in2

END ARCHITECTURE ideal;
--

-- Copyright Mentor Graphics Corporation 2001
-- Confidential Information Provided Under License Agreement for Internal Use Only

-- Digital clock with 50% duty cycle
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

ENTITY clock IS
  GENERIC (
    period : time);              -- Clock period
  
  PORT (
    clk_out    : OUT std_logic);
  
END ENTITY clock;

ARCHITECTURE ideal OF clock IS

BEGIN

-- clock process
    process
    begin
        clk_out <= '0';
        wait for period/2;
        clk_out <= '1';
        wait for period/2;
    end process;

END ARCHITECTURE ideal;
--

-- This digital clock allows user to specify the duty cycle using
-- the parameters "on_time" and "off_time"

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
use IEEE_proposed.mechanical_systems.all;

ENTITY clock_duty IS

  GENERIC (
    on_time : time := 20 us;
	off_time : time := 19.98 ms
 	); 
  
  PORT (
    clock_out    : OUT std_logic := '0');
  
END ENTITY clock_duty;

ARCHITECTURE ideal OF clock_duty IS

BEGIN

-- clock process
    process
    begin
        clock_out <= '1';
        wait for on_time;
        clock_out <= '0';
        wait for off_time;
    end process;

END ARCHITECTURE ideal;
--

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;

library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
use IEEE_proposed.mechanical_systems.all;

entity rc_clk is
    port(
        clk_100k : out std_logic;
        clk_6K : out std_logic;
        clk_50 : out std_logic
    );
end rc_clk;

architecture rc_clk of rc_clk is
    -- Component declarations
    -- Signal declarations
begin
    -- Signal assignments
    -- Component instances
    XCMP1 : entity work.clock(ideal)
        generic map(
            period => 10 us
        )
        port map(
            CLK_OUT => clk_100k
        );
    XCMP2 : entity work.clock(ideal)
        generic map(
            period => 150 us
        )
        port map(
            CLK_OUT => clk_6K
        );
    clk_50Hz : entity work.clock_duty(ideal)
        generic map(
            on_time => 20 us,
            off_time => 19.98 ms
        )
        port map(
            CLOCK_OUT => clk_50
        );
end rc_clk;
--

-- This model counts the number of input clock transitions and outputs
-- a '1' when this number equals the value of the user-defined constant 'count'

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
use IEEE_proposed.mechanical_systems.all;

entity bit_cnt is
  generic (
	count : integer -- User-defined value to count up to
	);             
port
(
      bit_in : in std_logic ;
      clk : in std_logic ;
      dly_out : out std_logic 
);
end bit_cnt;

architecture behavioral of bit_cnt is
begin
	serial_clock : process is
	begin
		wait until bit_in'event AND (bit_in = '1' OR bit_in = 'H');
	  	FOR i IN 0 to count LOOP -- Loop for 'count' clock transitions
	  		wait until clk'event AND (clk = '1' OR clk = 'H');
     	END LOOP ;
		dly_out <= '1'; -- After count is reached, set output high
		wait until bit_in'event AND (bit_in = '0' OR bit_in = 'L');
		dly_out <= '0'; -- Reset output to '0' on next clock input
	end process serial_clock;
end;
--

--//////////////////////////////////////////////////////////////////
-- NOTE: This is an intermediate file for HDL inspection only.
--       Please make all changes to C:\Scott\examples\ex_CS5\design_definition\graphics\state_mach1.sdg.
--       Generated by sde2hdl version 16.1.0.2
--//////////////////////////////////////////////////////////////////

LIBRARY IEEE;
USE IEEE.std_logic_1164.all;
USE IEEE.std_logic_arith.all;
LIBRARY IEEE_proposed;
USE IEEE_proposed.electrical_systems.all;
USE IEEE_proposed.mechanical_systems.all;

ENTITY state_mach1 IS
    PORT (
          a2d_eoc : IN std_logic;
          clk_50 : IN std_logic;
          clk_100k : IN std_logic;
          clk_6k : IN std_logic;
          ser_done : IN std_logic;
          ch_sel : OUT std_logic;
          frm_gen : OUT std_logic;
          a2d_oe : OUT std_logic;
          a2d_start : OUT std_logic;
          p2s_oe : OUT std_logic;
          p2s_load : OUT std_logic;
          parity_oe : OUT std_logic;
          ser_cnt : OUT std_logic;
          p2s_clr : OUT std_logic);

END state_mach1;

ARCHITECTURE state_diagram OF state_mach1 IS

    ATTRIBUTE ENUM_TYPE_ENCODING: STRING;

    TYPE TYP_state_mach1_sm1 IS (V_begin, frm_rd, ser_oe, ch1, data_en, tdm_oe, ch2
           , load, ad_ch2, delay);
    SIGNAL CS_state_mach1_sm1, NS_state_mach1_sm1 : TYP_state_mach1_sm1;

    SIGNAL FB_frm_gen : std_logic;
    SIGNAL FB_p2s_load : std_logic;
    SIGNAL FB_ch_sel : std_logic;

BEGIN 
    frm_gen <= FB_frm_gen ;
    p2s_load <= FB_p2s_load ;
    ch_sel <= FB_ch_sel ;

sm1: 
  PROCESS (CS_state_mach1_sm1, clk_50, FB_frm_gen, FB_p2s_load, ser_done, a2d_eoc, FB_ch_sel) 
  BEGIN

    CASE CS_state_mach1_sm1 IS
      WHEN V_begin =>
        FB_frm_gen <= ('1');
        a2d_start <= ('0');
        a2d_oe <= ('0');
        FB_p2s_load <= ('0');
        p2s_clr <= ('0');
        p2s_oe <= ('0');
        FB_ch_sel <= ('0');
        parity_oe <= ('0');
        ser_cnt <= ('0');

        IF ((FB_frm_gen = '1')) THEN
          NS_state_mach1_sm1 <= frm_rd;
        ELSE
          NS_state_mach1_sm1 <= V_begin;
        END IF;

      WHEN frm_rd =>
        FB_p2s_load <= ('1');

        IF ((FB_p2s_load = '1')) THEN
          NS_state_mach1_sm1 <= ser_oe;
        ELSE
          NS_state_mach1_sm1 <= frm_rd;
        END IF;

      WHEN ser_oe =>
        p2s_oe <= ('1');
        FB_frm_gen <= ('0');
        FB_p2s_load <= ('0');
        ser_cnt <= ('1');

        IF ((ser_done = '1')) THEN
          NS_state_mach1_sm1 <= ch1;
        ELSE
          NS_state_mach1_sm1 <= ser_oe;
        END IF;

      WHEN ch1 =>
        p2s_oe <= ('0');
        FB_ch_sel <= ('0');
        a2d_start <= ('1');
        ser_cnt <= ('0');

        IF ((a2d_eoc = '1')) THEN
          NS_state_mach1_sm1 <= data_en;
        ELSE
          NS_state_mach1_sm1 <= ch1;
        END IF;

      WHEN data_en =>
        a2d_start <= ('0');
        a2d_oe <= ('1');
        parity_oe <= ('1');
          NS_state_mach1_sm1 <= load;

      WHEN tdm_oe =>
        a2d_oe <= ('0');
        parity_oe <= ('0');
        p2s_oe <= ('1');
        FB_p2s_load <= ('0');
        ser_cnt <= ('1');

        IF (((ser_done = '1') AND (FB_ch_sel = '0'))) THEN
          NS_state_mach1_sm1 <= ch2;
        ELSE
          NS_state_mach1_sm1 <= tdm_oe;
        END IF;

      WHEN ch2 =>
        p2s_oe <= ('0');
        ser_cnt <= ('0');
        FB_ch_sel <= ('1');
          NS_state_mach1_sm1 <= delay;

      WHEN load =>
        FB_p2s_load <= ('1');
          NS_state_mach1_sm1 <= tdm_oe;

      WHEN ad_ch2 =>
        a2d_start <= ('1');

        IF ((a2d_eoc = '1')) THEN
          NS_state_mach1_sm1 <= data_en;
        ELSE
          NS_state_mach1_sm1 <= ad_ch2;
        END IF;

      WHEN delay =>
          NS_state_mach1_sm1 <= ad_ch2;

    END CASE;

  END PROCESS; 

sm1_CTL:
  PROCESS (clk_100k, clk_50)
  BEGIN

    IF (clk_100k'event AND clk_100k='1')  THEN 
        IF (clk_50= '1' ) THEN 
            CS_state_mach1_sm1 <= V_begin;
        ELSE
            CS_state_mach1_sm1 <= NS_state_mach1_sm1;
        END IF;
    END IF;

  END PROCESS;


END state_diagram;
--

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;

library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
use IEEE_proposed.mechanical_systems.all;

entity sm_cnt is
    port(
        a2d_eoc : in std_logic;
        clk_50 : in std_logic;
        clk_100k : in std_logic;
        clk_6k : in std_logic;
        p2s_load : out std_logic;
        p2s_oe : out std_logic;
        parity_oe : out std_logic;
        a2d_start : out std_logic;
        a2d_oe : out std_logic;
        frm_gen : out std_logic;
        ch_sel : out std_logic;
        p2s_clr : out std_logic
    );
end sm_cnt;

architecture sm_cnt of sm_cnt is
    -- Component declarations
    -- Signal declarations
    signal ser_done : std_logic;
    signal serial_cnt : std_logic;
begin
    -- Signal assignments
    -- Component instances
    bit_cnt1 : entity work.bit_cnt(behavioral)
        generic map(
            count => 15
        )
        port map(
            bit_in => serial_cnt,
            clk => clk_6k,
            dly_out => ser_done
        );
    state_mach16 : entity work.state_mach1
        port map(
            ser_cnt => serial_cnt,
            ch_sel => ch_sel,
            frm_gen => frm_gen,
            a2d_oe => a2d_oe,
            a2d_start => a2d_start,
            parity_oe => parity_oe,
            p2s_oe => p2s_oe,
            p2s_load => p2s_load,
            p2s_clr => p2s_clr,
            clk_6k => clk_6k,
            clk_100k => clk_100k,
            clk_50 => clk_50,
            a2d_eoc => a2d_eoc,
            ser_done => ser_done
        );
end sm_cnt;
--

-- Copyright Mentor Graphics Corporation 2001
-- Confidential Information Provided Under License Agreement for Internal Use Only
--  Analog to Digital Converter (Successive Aproximation Register) model with sar architecture (a2d_nbit.vhd)
--DESCRIPTION:
--
--This is a VHDL-AMS model of a simple analog to digital converter. The model
--describes the general behavior of A/D converters for system level design and
--verification.
--The format of the digital output is binary coding.
--
--N.B, dout(n-1) is the MSB while dout(0) is the LSB.
--

-- Use IEEE natures and packages
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
use IEEE_proposed.mechanical_systems.all;

entity a2d_nbit is
 generic (
  Vmax: REAL := 5.0 ;   -- ADC's maximum range
  Nbits: INTEGER := 10 ;  -- number bits in ADC's output
  delay: TIME := 10 us    -- ADC's conversion time
 );
 
port (
  signal start: in std_logic ; -- Start signal
  signal clk: in std_logic ;   -- Strobe clock
  signal oe: in std_logic ; -- Output enable
  terminal ain: ELECTRICAL ;  -- ADC's analog input terminal
  signal eoc: out std_logic := '0' ; -- End Of Conversion pin
  signal dout: out std_logic_vector(0 to (Nbits-1))); -- ADC's digital output signal
end entity a2d_nbit;

architecture sar of a2d_nbit is

  type states is (input, convert, output) ; -- Three states of A2D Conversion
  constant bit_range : INTEGER := Nbits-1 ; -- Bit range for dtmp and dout
  quantity Vin across Iin through ain to electrical_ref;     -- ADC's input branch

begin

 sa_adc: process

  variable thresh: REAL := Vmax ; -- Threshold to test input voltage against
  variable Vtmp: REAL := Vin ; -- Snapshot of input voltage when conversion starts
  variable dtmp: std_logic_vector(0 to (Nbits-1)); -- Temp. output data
  variable status: states := input ; -- Begin with "input" CASE 
  variable bit_cnt: integer := Nbits -1 ;

 begin
 CASE status is
	when input => -- Read input voltages when start goes high
		wait on start until start = '1' or start = 'H' ;
  		thresh := Vmax ;
  		Vtmp := Vin ;
		eoc <= '0' ;
		status := convert ; -- Go to convert state
	when convert => -- Begin successive approximation conversion
			thresh := thresh / 2.0 ; -- Get value of MSB
			wait on clk until clk = '1' OR clk = 'H';
			if Vtmp > thresh then
				dtmp(bit_cnt) := '1' ;
				Vtmp := Vtmp - thresh ;
			else
				dtmp(bit_cnt) := '0' ;
			end if ;
			bit_cnt := bit_cnt - 1 ;
			if (bit_cnt + 1) < 1 then
				status := output ; -- Go to output state
			end if;
	when output => -- Wait for output enable, then put data on output pins
		eoc <= '1' after delay ;
		wait on oe until oe = '1' OR oe = 'H' ;
			FOR i in bit_range DOWNTO 0 LOOP
				dout(i) <= dtmp(i) ;
			END LOOP ;
		wait on oe until oe = '0' OR oe = 'L' ; -- Hi Z when OE is low
			FOR i in bit_range DOWNTO 0 LOOP
				dout <= "ZZZZZZZZZZ" ;
			END LOOP ;
		bit_cnt := bit_range ;
		status := input ; -- Set up for next conversion
	END CASE ;
 end process sa_adc ;

 Iin == 0.0 ; -- Ideal input draws no current

end architecture sar ;
--

-- Parallel input/serial output shift register
-- With 4 trailing zeros

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
use IEEE_proposed.mechanical_systems.all;

entity shift_reg is
generic ( td : time := 0 ns); 

port
(
      bus_in : in std_logic_vector ; -- Input bus
      clk : in std_logic ; -- Shift clock
      oe : in std_logic ; -- Output enable
      ser_out : out std_logic := '0'; -- Output port
      load : in std_logic ; -- Parallel input load
	clr : in std_logic -- Clear register
);

end entity shift_reg;

architecture behavioral of shift_reg is
begin

control_proc : process
  VARIABLE bit_val : std_logic_vector(11 downto 0);  -- Default 12-bit input
  begin

	IF (clr = '1' OR clr = 'H') then
    		bit_val := "000000000000"; -- Set all input bits to zero
	ELSE
  		wait until load'event AND (load = '1' OR load = 'H');
		FOR i IN bus_in'high DOWNTO bus_in'low LOOP
        		bit_val(i) := bus_in(i) ; -- Transfer input data to variable
     		END LOOP ;
	END IF;

	wait until oe'event AND (oe = '1' OR oe = 'H'); -- Shift if output enabled
	FOR i IN bit_val'high DOWNTO bit_val'low LOOP
		wait until clk'event AND (clk = '1' OR clk = 'H');
      	ser_out <= bit_val(i) ;
	END LOOP ;

	  FOR i IN 1 TO 4 LOOP -- This loop pads the serial output with 4 zeros	
		wait until clk'event AND (clk = '1' OR clk = 'H');
		ser_out <= '0';
	  END LOOP;

END process;

end architecture behavioral;
--

-- This model generates a 12-bit data frame synchronization code

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
use IEEE_proposed.mechanical_systems.all;

entity frame_gen is
port
(
      oe : in std_logic := '0';
	sync_out : out std_logic_vector (11 downto 0) := "ZZZZZZZZZZZZ");

end entity frame_gen;

architecture simple of frame_gen is
begin
	enbl: PROCESS
  		BEGIN 
  			WAIT ON OE;
			IF OE = '1' THEN
				sync_out <= "010101010101"; -- Sync code
			ELSE
				sync_out <= "ZZZZZZZZZZZZ";
			END IF;
  	END PROCESS;
end architecture simple;
--

-- Copyright Mentor Graphics Corporation 2001
-- Confidential Information Provided Under License Agreement for Internal Use Only

-- Two input XOR gate
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

ENTITY xor2 IS
  GENERIC (
    delay : time := 0 ns);              -- Delay time
  
  PORT (
    in1, in2 : IN  std_logic;
    output    : OUT std_logic);
  
END ENTITY xor2;

ARCHITECTURE ideal OF xor2 IS
BEGIN
   output <= in1 XOR in2 AFTER delay;
END ARCHITECTURE ideal;
--

-- Copyright Mentor Graphics Corporation 2001
-- Confidential Information Provided Under License Agreement for Internal Use Only

-- level_set_tri.vhd 
-- If OE = '1' set digital output "level" with parameter "logic_val" (default is 'Z')
-- If OE = '0' set output to high impedance

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

ENTITY level_set_tri IS

  GENERIC (
    logic_val : std_logic := 'Z');     

  PORT (
  	OE : IN std_logic;
    level : OUT std_logic := 'Z');

END ENTITY level_set_tri;

-- Simple architecture

ARCHITECTURE ideal OF level_set_tri IS
BEGIN
  oe_ctl: PROCESS
  BEGIN
	WAIT ON OE;
 	IF OE = '1' THEN 
   		level <= logic_val;
	ELSE
		level <= 'Z';
	END IF;
  END PROCESS;
  
END ARCHITECTURE ideal;

--

-- Copyright Mentor Graphics Corporation 2001
-- Confidential Information Provided Under License Agreement for Internal Use Only

-- Simple Tri-state Buffer with delay time
-- If OE = 1, output = input after delay
-- If OE /= 1, output = Z after delay

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

ENTITY buffer_tri IS
  GENERIC (
    delay : time := 0 ns);  -- Delay time
  
  PORT (
    input : IN  std_logic;
	OE : IN std_logic;
    output : OUT std_logic);
  
END ENTITY buffer_tri;

ARCHITECTURE ideal OF buffer_tri IS
BEGIN
  oe_ctl: PROCESS
  BEGIN
	WAIT ON OE, input;
 	IF OE = '1' THEN 
   		output <= input AFTER delay;
	ELSE
		output <= 'Z' AFTER delay;
	END IF;
  END PROCESS;
END ARCHITECTURE ideal;
--

-- Copyright Mentor Graphics Corporation 2001
-- Confidential Information Provided Under License Agreement for Internal Use Only

-- ideal one bit D/A converter

LIBRARY IEEE_proposed;
USE IEEE_proposed.electrical_systems.ALL;

LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;

ENTITY d2a_bit IS
  GENERIC (vlow : real :=0.0;           -- output high voltage
           vhigh : real :=5.0);         -- output low voltage
  PORT	  (D : IN std_logic;            -- digital (std_logic) intout
           TERMINAL A : electrical);    -- analog (electrical) output
END ENTITY d2a_bit;

ARCHITECTURE ideal OF d2a_bit IS
  QUANTITY vout ACROSS iout THROUGH A TO ELECTRICAL_REF;
  SIGNAL vin : real := 0.0;

  BEGIN
    vin <= vhigh WHEN D = '1' ELSE vlow;
    -- Use 'RAMP for discontinuous signal
    vout == vin'RAMP(1.0e-9);      

END ARCHITECTURE ideal;
--

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;

library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
use IEEE_proposed.mechanical_systems.all;

entity parity_gen is
    port(
        parity : in std_logic_vector(1 to 10);
        oe : in std_logic;
        parity_out : out std_logic_vector(0 to 11)
    );
end parity_gen;

architecture parity_gen of parity_gen is
    -- Component declarations
    -- Signal declarations
    terminal par_bit_gen_a : electrical;
    signal XSIG010002 : std_logic;
    signal XSIG010003 : std_logic;
    signal XSIG010004 : std_logic;
    signal XSIG010005 : std_logic;
    signal XSIG010006 : std_logic;
    signal XSIG010007 : std_logic;
    signal XSIG010008 : std_logic;
    signal XSIG010009 : std_logic;
    signal XSIG010098 : std_logic;
begin
    -- Signal assignments
    -- Component instances
    XCMP1 : entity work.xor2(ideal)
        port map(
            in1 => parity(1),
            in2 => parity(2),
            output => XSIG010002
        );
    XCMP2 : entity work.xor2(ideal)
        port map(
            in1 => parity(3),
            in2 => parity(4),
            output => XSIG010003
        );
    XCMP3 : entity work.xor2(ideal)
        port map(
            in1 => parity(5),
            in2 => parity(6),
            output => XSIG010004
        );
    XCMP4 : entity work.xor2(ideal)
        port map(
            in1 => parity(7),
            in2 => parity(8),
            output => XSIG010005
        );
    XCMP5 : entity work.xor2(ideal)
        port map(
            in1 => parity(9),
            in2 => parity(10),
            output => XSIG010008
        );
    XCMP6 : entity work.xor2(ideal)
        port map(
            in1 => XSIG010002,
            in2 => XSIG010003,
            output => XSIG010006
        );
    XCMP7 : entity work.xor2(ideal)
        port map(
            in1 => XSIG010004,
            in2 => XSIG010005,
            output => XSIG010007
        );
    XCMP8 : entity work.xor2(ideal)
        port map(
            in1 => XSIG010006,
            in2 => XSIG010007,
            output => XSIG010009
        );
    XCMP9 : entity work.xor2(ideal)
        port map(
            in1 => XSIG010009,
            in2 => XSIG010008,
            output => XSIG010098
        );
    XCMP18 : entity work.level_set_tri(ideal)
        generic map(
            logic_val => '1'
        )
        port map(
            level => parity_out(11),
            oe => oe
        );
    XCMP19 : entity work.buffer_tri(ideal)
        port map(
            input => parity(1),
            output => parity_out(1),
            oe => oe
        );
    XCMP20 : entity work.buffer_tri(ideal)
        port map(
            input => parity(2),
            output => parity_out(2),
            oe => oe
        );
    XCMP21 : entity work.buffer_tri(ideal)
        port map(
            input => parity(3),
            output => parity_out(3),
            oe => oe
        );
    XCMP22 : entity work.buffer_tri(ideal)
        port map(
            input => parity(4),
            output => parity_out(4),
            oe => oe
        );
    XCMP23 : entity work.buffer_tri(ideal)
        port map(
            input => parity(5),
            output => parity_out(5),
            oe => oe
        );
    XCMP24 : entity work.buffer_tri(ideal)
        port map(
            input => parity(6),
            output => parity_out(6),
            oe => oe
        );
    XCMP25 : entity work.buffer_tri(ideal)
        port map(
            input => parity(7),
            output => parity_out(7),
            oe => oe
        );
    XCMP26 : entity work.buffer_tri(ideal)
        port map(
            input => parity(8),
            output => parity_out(8),
            oe => oe
        );
    XCMP27 : entity work.buffer_tri(ideal)
        port map(
            input => parity(9),
            output => parity_out(9),
            oe => oe
        );
    XCMP28 : entity work.buffer_tri(ideal)
        port map(
            input => parity(10),
            output => parity_out(10),
            oe => oe
        );
    XCMP29 : entity work.buffer_tri(ideal)
        port map(
            input => XSIG010098,
            output => parity_out(0),
            oe => oe
        );
    XCMP30 : entity work.d2a_bit(ideal)
        port map(
            D => XSIG010098,
            A => par_bit_gen_a
        );
end parity_gen;
--

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;

library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
use IEEE_proposed.mechanical_systems.all;

entity tdm_encoder is
    port(
        clk : in std_logic;
        p2s_oe : in std_logic;
        p2s_load : in std_logic;
        frm_gen : in std_logic;
        parity_oe : in std_logic;
        tdm_out : out std_logic;
        p2s_clr : in std_logic;
        a2d_data : in std_logic_vector(1 to 10)
    );
end tdm_encoder;

architecture tdm_encoder of tdm_encoder is
    -- Component declarations
    -- Signal declarations
    signal sync_par : std_logic_vector(0 to 11);
begin
    -- Signal assignments
    -- Component instances
    p2s1 : entity work.shift_reg(behavioral)
        port map(
            bus_in => sync_par,
            clk => clk,
            oe => p2s_oe,
            ser_out => tdm_out,
            load => p2s_load,
            clr => p2s_clr
        );
    sync_gen1 : entity work.frame_gen(simple)
        port map(
            oe => frm_gen,
            sync_out => sync_par
        );
    par_gen1 : entity work.parity_gen
        port map(
            parity => a2d_data,
            parity_out => sync_par,
            oe => parity_oe
        );
end tdm_encoder;
--

-- Manchester Encoder

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

ENTITY menc_rsc IS
	
	port ( 	dig_in : in STD_LOGIC;  -- digital input
		clk : in STD_LOGIC;     -- TX internal clock
		reset: in STD_LOGIC;  -- not reset
--	       	bit_out : inout real);  -- real output
		bit_out : out std_logic);  -- real output
	       
END ENTITY menc_rsc;

ARCHITECTURE bhv OF menc_rsc IS
	 
--	 signal bhigh:real:= 1.0;   -- bit encoding
--	 signal blow:real:= -1.0;   -- bit encoding
--	 signal bnormal:real:=0.0;  -- bit encoding
	 signal bit1:STD_LOGIC;
	 signal bhigh:std_logic:= '1';   -- bit encoding
	 signal blow:std_logic:= '0';   -- bit encoding
	 
begin

--	proc1: process  (dig_in, clk, bit1,bhigh,blow,bnormal) 
	proc1: process  (dig_in, clk, bit1,bhigh,blow) 
	begin
	 	 	
	if (reset = '1') then
	   bit1 <= '0';
	else
	   bit1 <= dig_in XOR clk;  -- manchester encoding 
	end if;
	
	if (bit1 = '1') then
	   bit_out <= bhigh;
	else 
		bit_out <= blow;
--	elsif bit1 = '0' then
--	   bit_out <= blow;
--	else
--	   bit_out <= bnormal;
	end if;
	  
	end process;
	
end architecture bhv;
	
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;

library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
use IEEE_proposed.mechanical_systems.all;

entity Digitize_Encode_Man is
    port(
        tdm_out : out std_logic;
        terminal ch1_in : electrical;
        terminal ch2_in : electrical
    );
end Digitize_Encode_Man;

architecture Digitize_Encode_Man of Digitize_Encode_Man is
    -- Component declarations
    -- Signal declarations
    terminal a2d_ana_in : electrical;
    signal ch_bus : std_logic_vector(1 to 10);
    signal clk_6K : std_logic;
    signal dig_in : std_logic;
    signal frm_gen_ctl : std_logic;
    signal p2s_clr : std_logic;
    signal p2s_load : std_logic;
    signal p2s_oe : std_logic;
    signal par_oe : std_logic;
    signal reset : std_logic;
    signal reset_m : std_logic;
    signal start_a2d1 : std_logic;
    signal sw_ctl : std_logic;
    signal XSIG010091 : std_logic;
    signal XSIG010190 : std_logic;
    signal XSIG010196 : std_logic;
begin
    -- Signal assignments
    -- Component instances
    A_SWITCH1 : entity work.switch_dig_2in(ideal)
        port map(
            p_in1 => ch1_in,
            p_out => a2d_ana_in,
            sw_state => sw_ctl,
            p_in2 => ch2_in
        );
    rc_clk2 : entity work.rc_clk
        port map(
            clk_50 => reset,
            clk_6K => clk_6K,
            clk_100k => XSIG010190
        );
    sm_xmtr1 : entity work.sm_cnt
        port map(
            clk_100k => XSIG010190,
            a2d_start => start_a2d1,
            a2d_eoc => XSIG010091,
            p2s_oe => p2s_oe,
            p2s_load => p2s_load,
            ch_sel => sw_ctl,
            frm_gen => frm_gen_ctl,
            parity_oe => par_oe,
            a2d_oe => XSIG010196,
            clk_50 => reset,
            clk_6k => clk_6K,
            p2s_clr => p2s_clr
        );
    a2d1 : entity work.a2d_nbit(sar)
        generic map(
            Vmax => 4.8
        )
        port map(
            dout => ch_bus,
            ain => a2d_ana_in,
            clk => XSIG010190,
            start => start_a2d1,
            eoc => XSIG010091,
            oe => XSIG010196
        );
    tdm_enc1 : entity work.tdm_encoder
        port map(
            clk => clk_6K,
            p2s_oe => p2s_oe,
            tdm_out => dig_in,
            p2s_load => p2s_load,
            a2d_data => ch_bus,
            frm_gen => frm_gen_ctl,
            parity_oe => par_oe,
            p2s_clr => p2s_clr
        );
    menc_rsc3 : entity work.menc_rsc(bhv)
        port map(
            dig_in => dig_in,
            clk => clk_6K,
            reset => reset_m,
            bit_out => tdm_out
        );
    XCMP90 : entity work.clock_duty(ideal)
        generic map(
            off_time => 19.98 sec
        )
        port map(
            CLOCK_OUT => reset_m
        );
end Digitize_Encode_Man;
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
library IEEE;
use ieee.math_real.all;

entity lpf_2_e is
	generic (
		k: real := 1.0;  			-- Gain multiplier
		f1: real := 10.0;			-- First break frequency (pole)
		f2: real := 100.0);			-- Second break frequency (pole)
	port 	(	terminal input: electrical;
			terminal output: electrical);
end entity lpf_2_e;

architecture simple of lpf_2_e is
  QUANTITY vin ACROSS input TO ELECTRICAL_REF;
  QUANTITY vout ACROSS iout THROUGH output TO ELECTRICAL_REF;

	quantity vin_temp : real;
	constant w1 : real := f1*math_2_pi;
	constant w2 : real := f2*math_2_pi;
--	constant num : real := k;
	constant num : real_vector := (0 => w1*w2*k);  -- 0=> is needed to give
                                                   -- index when only a single
                                                   -- element is used.
	constant den : real_vector := (w1*w2, w1+w2, 1.0);
begin
	vin_temp == vin;   -- intermediate variable (vin) req'd for now
	vout == vin_temp'ltf(num, den);
end architecture simple;

--

-- Copyright Mentor Graphics Corporation 2001
-- Confidential Information Provided Under License Agreement for Internal Use Only

-- Two input AND gate
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

ENTITY and2 IS
  GENERIC (
    delay : time := 0 ns);              -- Delay time
  
  PORT (
    in1, in2 : IN  std_logic;
    output    : OUT std_logic);
  
END ENTITY and2;

ARCHITECTURE ideal OF and2 IS
BEGIN
   output <= in1 AND in2 AFTER delay;
END ARCHITECTURE ideal;
--

-- D Flip Flop with reset (negative edge triggered)

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

ENTITY d_latch_n_edge_rst IS
	GENERIC (
		delay : time := 0 ns);  -- Delay time
  
	PORT (
		data, clk 	: IN std_logic;
		q 	: OUT std_logic := '0';
		qn	: OUT std_logic := '1';
		rst	: IN std_logic := '0');  -- reset 
	   
END ENTITY d_latch_n_edge_rst ;

ARCHITECTURE behav OF d_latch_n_edge_rst IS
BEGIN
 
 data_in : PROCESS(clk, rst) IS  

   BEGIN
     IF clk = '0' AND clk'event AND rst /= '1' THEN
	     q  <= data     	AFTER delay;
         qn <= NOT data 	AFTER delay;
     ELSIF rst = '1' THEN
         q <= '0';
         qn <= '1';
     END IF;
     
   END PROCESS data_in;  -- End of process data_in

END ARCHITECTURE behav;
--

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;

library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
use IEEE_proposed.mechanical_systems.all;

entity counter_12 is
    port(
        cnt : out std_logic_vector(0 to 11);
        reset : in std_logic;
        enable : in std_logic;
        clk : in std_logic
    );
end counter_12;

architecture counter_12 of counter_12 is
    -- Component declarations
    -- Signal declarations
    signal cdb2vhdl_tmp_1 : std_logic_vector(0 to 11);
    signal XSIG010078 : std_logic;
    signal XSIG010081 : std_logic;
    signal XSIG010083 : std_logic;
    signal XSIG010085 : std_logic;
    signal XSIG010087 : std_logic;
    signal XSIG010101 : std_logic;
    signal XSIG010102 : std_logic;
    signal XSIG010103 : std_logic;
    signal XSIG010104 : std_logic;
    signal XSIG010115 : std_logic;
    signal XSIG010116 : std_logic;
    signal XSIG010117 : std_logic;
    signal XSIG010132 : std_logic;
begin
    -- Signal assignments
    cnt(0) <= cdb2vhdl_tmp_1(0);
    cnt(1) <= cdb2vhdl_tmp_1(1);
    cnt(2) <= cdb2vhdl_tmp_1(2);
    cnt(3) <= cdb2vhdl_tmp_1(3);
    cnt(4) <= cdb2vhdl_tmp_1(4);
    cnt(5) <= cdb2vhdl_tmp_1(5);
    cnt(6) <= cdb2vhdl_tmp_1(6);
    cnt(7) <= cdb2vhdl_tmp_1(7);
    cnt(8) <= cdb2vhdl_tmp_1(8);
    cnt(9) <= cdb2vhdl_tmp_1(9);
    cnt(10) <= cdb2vhdl_tmp_1(10);
    cnt(11) <= cdb2vhdl_tmp_1(11);
    -- Component instances
    XCMP92 : entity work.and2(ideal)
        port map(
            in1 => clk,
            in2 => enable,
            output => XSIG010132
        );
    XCMP93 : entity work.d_latch_n_edge_rst(behav)
        port map(
            CLK => XSIG010132,
            DATA => XSIG010078,
            QN => XSIG010078,
            Q => cdb2vhdl_tmp_1(0),
            RST => reset
        );
    XCMP94 : entity work.d_latch_n_edge_rst(behav)
        port map(
            CLK => cdb2vhdl_tmp_1(0),
            DATA => XSIG010081,
            QN => XSIG010081,
            Q => cdb2vhdl_tmp_1(1),
            RST => reset
        );
    XCMP95 : entity work.d_latch_n_edge_rst(behav)
        port map(
            CLK => cdb2vhdl_tmp_1(1),
            DATA => XSIG010083,
            QN => XSIG010083,
            Q => cdb2vhdl_tmp_1(2),
            RST => reset
        );
    XCMP96 : entity work.d_latch_n_edge_rst(behav)
        port map(
            CLK => cdb2vhdl_tmp_1(2),
            DATA => XSIG010085,
            QN => XSIG010085,
            Q => cdb2vhdl_tmp_1(3),
            RST => reset
        );
    XCMP97 : entity work.d_latch_n_edge_rst(behav)
        port map(
            CLK => cdb2vhdl_tmp_1(3),
            DATA => XSIG010087,
            QN => XSIG010087,
            Q => cdb2vhdl_tmp_1(4),
            RST => reset
        );
    XCMP98 : entity work.d_latch_n_edge_rst(behav)
        port map(
            CLK => cdb2vhdl_tmp_1(4),
            DATA => XSIG010101,
            QN => XSIG010101,
            Q => cdb2vhdl_tmp_1(5),
            RST => reset
        );
    XCMP99 : entity work.d_latch_n_edge_rst(behav)
        port map(
            CLK => cdb2vhdl_tmp_1(5),
            DATA => XSIG010102,
            QN => XSIG010102,
            Q => cdb2vhdl_tmp_1(6),
            RST => reset
        );
    XCMP100 : entity work.d_latch_n_edge_rst(behav)
        port map(
            CLK => cdb2vhdl_tmp_1(6),
            DATA => XSIG010103,
            QN => XSIG010103,
            Q => cdb2vhdl_tmp_1(7),
            RST => reset
        );
    XCMP101 : entity work.d_latch_n_edge_rst(behav)
        port map(
            CLK => cdb2vhdl_tmp_1(7),
            DATA => XSIG010104,
            QN => XSIG010104,
            Q => cdb2vhdl_tmp_1(8),
            RST => reset
        );
    XCMP102 : entity work.d_latch_n_edge_rst(behav)
        port map(
            CLK => cdb2vhdl_tmp_1(8),
            DATA => XSIG010115,
            QN => XSIG010115,
            Q => cdb2vhdl_tmp_1(9),
            RST => reset
        );
    XCMP103 : entity work.d_latch_n_edge_rst(behav)
        port map(
            CLK => cdb2vhdl_tmp_1(9),
            DATA => XSIG010116,
            QN => XSIG010116,
            Q => cdb2vhdl_tmp_1(10),
            RST => reset
        );
    XCMP104 : entity work.d_latch_n_edge_rst(behav)
        port map(
            CLK => cdb2vhdl_tmp_1(10),
            DATA => XSIG010117,
            QN => XSIG010117,
            Q => cdb2vhdl_tmp_1(11),
            RST => reset
        );
end counter_12;
--

-- Copyright Mentor Graphics Corporation 2001
-- Confidential Information Provided Under License Agreement for Internal Use Only

-- ideal one bit A/D converter

LIBRARY IEEE;
USE IEEE.math_real.ALL;
USE IEEE.std_logic_1164.ALL;

LIBRARY IEEE_proposed;
USE IEEE_proposed.electrical_systems.ALL;

ENTITY a2d_bit IS
  
  GENERIC (
    thres : real := 2.5);    -- Threshold to determine logic output

  PORT (
    TERMINAL a :     electrical;   -- analog input
    SIGNAL   d : OUT std_logic);   -- digital (std_logic) output
  
END ENTITY a2d_bit;


ARCHITECTURE ideal OF a2d_bit IS

  QUANTITY vin ACROSS a;
  
  BEGIN  -- threshold
-- Process needed to detect threshold crossing and assign output (d)
    PROCESS (vin'ABOVE(thres)) IS
      BEGIN  -- PROCESS
      IF vin'ABOVE(thres) THEN
         d <= '1';
      ELSE
         d <= '0';  
      END IF;
    END PROCESS;

END ideal;

-- Copyright Mentor Graphics Corporation 2001
-- Confidential Information Provided Under License Agreement for Internal Use Only

-- Digital clock with 50% duty cycle and enable pin
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

ENTITY clock_en IS
  GENERIC (
    pw : time);              -- Clock pulse width
  
  PORT (
	enable : IN std_logic ;
	clock_out : INOUT std_logic := '0');
  
END ENTITY clock_en;

ARCHITECTURE ideal OF clock_en IS

BEGIN

-- clock process
    process (clock_out, enable) is
    begin
   	if clock_out = '0' AND enable = '1' THEN
        clock_out <= '1' after pw, '0' after 2*pw;
	end if;
    end process;

END ARCHITECTURE ideal;
--

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

-- Copyright Mentor Graphics Corporation 2001
-- Confidential Information Provided Under License Agreement for Internal Use Only

-- Two input OR gate
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

ENTITY or2 IS
  GENERIC (
    delay : time := 0 ns);              -- Delay time
  
  PORT (
    in1, in2 : IN  std_logic;
    output    : OUT std_logic);
  
END ENTITY or2;

ARCHITECTURE ideal OF or2 IS
BEGIN
   output <= in1 OR in2 AFTER delay;
END ARCHITECTURE ideal;
--

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
use IEEE_proposed.mechanical_systems.all;

ENTITY d2a_nbit IS

  GENERIC (
    vmax : real := 5.0;	-- High output
    vmin : real := 0.0;	-- Low output
    high_bit : integer := 9; -- High end of bit range for D/A
	low_bit : integer := 0); -- Low end of bit range for D/A

  PORT (
    SIGNAL   bus_in : IN STD_LOGIC_VECTOR;  -- variable width vector input
    SIGNAL latch : IN STD_LOGIC;
    TERMINAL ana_out  :    electrical);      -- analog output

END ENTITY d2a_nbit ;

ARCHITECTURE behavioral OF d2a_nbit IS

  SIGNAL   sout : real := 0.0;
  QUANTITY vout across iout through ana_out TO electrical_ref;

BEGIN  -- ARCHITECTURE behavioral

  proc : PROCESS

    VARIABLE v_sum  : real; -- Sum of voltage contribution from each bit
    VARIABLE delt_v : real; -- Represents the voltage value of each bit

  BEGIN
	WAIT UNTIL (latch'event and latch = '1'); -- Begin when latch goes high
  	v_sum     := vmin;
    	delt_v    := vmax - vmin;

    FOR i IN high_bit DOWNTO low_bit LOOP -- Perform the conversions
      delt_v  := delt_v / 2.0;
      IF bus_in(i) = '1' OR bus_in(i) = 'H' THEN
        v_sum := v_sum + delt_v;
      END IF;
    END LOOP;

    sout <= v_sum;
  END PROCESS;

  vout == sout'ramp(100.0E-9); -- Ensure continuous transition between levels

END ARCHITECTURE behavioral;

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;

library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
use IEEE_proposed.mechanical_systems.all;

entity pw2ana is
    port(
        terminal ana_out : electrical;
        terminal pw_in : electrical
    );
end pw2ana;

architecture pw2ana of pw2ana is
    -- Component declarations
    -- Signal declarations
    signal bus_servo : std_logic_vector(0 to 11);
    signal XSIG010008 : std_logic;
    signal XSIG010013 : std_logic;
    signal XSIG010019 : std_logic;
    signal XSIG010020 : std_logic;
    signal XSIG010021 : std_logic;
    signal XSIG010022 : std_logic;
begin
    -- Signal assignments
    -- Component instances
    counter_rudder : entity work.counter_12
        port map(
            enable => XSIG010022,
            cnt => bus_servo,
            reset => XSIG010021,
            clk => XSIG010008
        );
    XCMP3 : entity work.a2d_bit(ideal)
        port map(
            D => XSIG010022,
            A => pw_in
        );
    clk_en_rudder : entity work.clock_en(ideal)
        generic map(
            pw => 500 ns
        )
        port map(
            CLOCK_OUT => XSIG010008,
            enable => XSIG010022
        );
    XCMP5 : entity work.inverter(ideal)
        generic map(
            delay => 2 us
        )
        port map(
            input => XSIG010022,
            output => XSIG010013
        );
    XCMP8 : entity work.inverter(ideal)
        generic map(
            delay => 2 us
        )
        port map(
            input => XSIG010020,
            output => XSIG010021
        );
    XCMP9 : entity work.inverter(ideal)
        generic map(
            delay => 2 us
        )
        port map(
            input => XSIG010022,
            output => XSIG010019
        );
    or_rudder : entity work.or2(ideal)
        port map(
            in1 => XSIG010022,
            in2 => XSIG010019,
            output => XSIG010020
        );
    XCMP11 : entity work.d2a_nbit(behavioral)
        generic map(
            vmax => 4.8,
            high_bit => 9,
            low_bit => 0
        )
        port map(
            bus_in => bus_servo,
            ana_out => ana_out,
            latch => XSIG010013
        );
end pw2ana;
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

-- 12-bit digital comparator model
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
use IEEE_proposed.mechanical_systems.all;

entity dig_cmp is
port
(
      eq : out std_logic := '0';
	  in1 : in std_logic_vector (0 to 11);
	  in2 : in std_logic_vector (0 to 11);
	  latch_in1 : in std_logic := '0'; -- Currently unused
	  latch_in2 : in std_logic := '0';
	  cmp : in std_logic := '0';
	  clk : in std_logic
	  );

end entity dig_cmp ;

architecture simple of dig_cmp is

begin

	compare: PROCESS (latch_in2, cmp, clk) -- Sensitivity list
	variable in2_hold : std_logic_vector (0 to 11) := "000000000000";
  		BEGIN
		if latch_in2 = '1' then -- in2 data is latched and stored
			in2_hold := in2;
		end if;
		if cmp = '1' then
			if in1 = in2_hold then -- latched in2 checked against current in1
				eq <= '0';
			else eq <= '1';
			end if;
		end if;
  	END PROCESS;
end architecture simple;

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
-- Set/reset flip flop
-- When S goes high, Q is set high until reset
-- When R goes high, Q is set low until set
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
use IEEE_proposed.mechanical_systems.all;

entity sr_ff is
port
(
      S : in std_logic ;
      R : in std_logic ;
	Q : out std_logic 
);

end sr_ff ;

architecture simple of sr_ff is
begin

 set_reset: PROCESS(S, R) IS  

   BEGIN
--	assert S='1' nand R='1' -- Warning if both inputs are high
--		report "S and R are both active. Use with caution"
--		severity warning;
	if S'event AND S = '1' then
		Q <= '1';
	end if;
	if R'event AND R = '1' then
		Q <= '0';
	end if;     
   END PROCESS set_reset; 

end;
--

--//////////////////////////////////////////////////////////////////
-- NOTE: This is an intermediate file for HDL inspection only.
--       Please make all changes to C:\Scott\examples\ex_CS5\design_definition\graphics\state_mach_rcvr.sdg.
--       Generated by sde2hdl version 16.1.0.2
--//////////////////////////////////////////////////////////////////

LIBRARY IEEE;
USE IEEE.std_logic_1164.all;
USE IEEE.std_logic_arith.all;
LIBRARY IEEE_proposed;
USE IEEE_proposed.electrical_systems.all;
USE IEEE_proposed.mechanical_systems.all;
USE IEEE_proposed.fluidic_systems.all;
USE IEEE_proposed.thermal_systems.all;
USE IEEE_proposed.radiant_systems.all;
ENTITY state_mach_rcvr IS
    PORT (
          clk_50 : IN std_logic;
          clk_100k : IN std_logic;
          ser_done : IN std_logic;
          par_det : IN std_logic;
          frm_det : IN std_logic;
          clk_6k : IN std_logic;
          start_pulse : IN std_logic;
          dly_done : IN std_logic;
          s2p_rst : OUT std_logic;
          s2p_en : OUT std_logic;
          cnt1_en : OUT std_logic;
          cnt1_rst : OUT std_logic;
          cmp1_ltch1 : OUT std_logic;
          cmp1_ltch2 : OUT std_logic;
          cnt2_en : OUT std_logic;
          cnt2_rst : OUT std_logic;
          cmp2_ltch1 : OUT std_logic;
          cmp2_ltch2 : OUT std_logic;
          da_latch : OUT std_logic;
          ser_cnt : OUT std_logic;
          dly_cnt : OUT std_logic;
          par_oe : OUT std_logic);

END state_mach_rcvr;

ARCHITECTURE state_diagram OF state_mach_rcvr IS

    ATTRIBUTE ENUM_TYPE_ENCODING: STRING;

    TYPE TYP_state_mach_rcvr_sm1 IS (V_begin, cnt, ch1, rst1, ch2, rst2, cnt_cmp, rst_cnt
           , s_bit, par1, par2);
    SIGNAL CS_state_mach_rcvr_sm1, NS_state_mach_rcvr_sm1 : TYP_state_mach_rcvr_sm1;


BEGIN 

sm1: 
  PROCESS (CS_state_mach_rcvr_sm1, clk_50, frm_det, ser_done, start_pulse, dly_done, par_det) 
  BEGIN

    CASE CS_state_mach_rcvr_sm1 IS
      WHEN V_begin =>
        cnt1_en <= ('0');
        cnt1_rst <= ('1');
        cmp1_ltch1 <= ('0');
        cmp1_ltch2 <= ('0');
        cnt2_en <= ('0');
        cnt2_rst <= ('1');
        cmp2_ltch1 <= ('0');
        cmp2_ltch2 <= ('0');
        s2p_en <= ('1');
        s2p_rst <= ('0');
        da_latch <= ('0');
        ser_cnt <= ('0');
        dly_cnt <= ('0');
        par_oe <= ('0');

        IF ((frm_det = '1')) THEN
          NS_state_mach_rcvr_sm1 <= s_bit;
        ELSE
          NS_state_mach_rcvr_sm1 <= V_begin;
        END IF;

      WHEN cnt =>
        ser_cnt <= ('1');
        cnt1_rst <= ('0');
        cnt2_rst <= ('0');

        IF ((ser_done = '1')) THEN
          NS_state_mach_rcvr_sm1 <= par1;
        ELSE
          NS_state_mach_rcvr_sm1 <= cnt;
        END IF;

      WHEN ch1 =>
        cmp1_ltch2 <= ('1');
        ser_cnt <= ('0');
        dly_cnt <= ('1');

        IF (((start_pulse = '1') AND (dly_done = '1'))) THEN
          NS_state_mach_rcvr_sm1 <= rst1;
        ELSE
          NS_state_mach_rcvr_sm1 <= ch1;
        END IF;

      WHEN rst1 =>
        cmp1_ltch2 <= ('0');
        ser_cnt <= ('1');
        dly_cnt <= ('0');
        par_oe <= ('0');

        IF ((ser_done = '1')) THEN
          NS_state_mach_rcvr_sm1 <= par2;
        ELSE
          NS_state_mach_rcvr_sm1 <= rst1;
        END IF;

      WHEN ch2 =>
        cmp2_ltch2 <= ('1');
        ser_cnt <= ('0');
        da_latch <= ('1');
          NS_state_mach_rcvr_sm1 <= rst2;

      WHEN rst2 =>
        cmp2_ltch2 <= ('0');
        s2p_en <= ('0');
        par_oe <= ('0');
        da_latch <= ('0');
          NS_state_mach_rcvr_sm1 <= cnt_cmp;

      WHEN cnt_cmp =>
        cnt1_en <= ('1');
        cmp1_ltch1 <= ('1');
        cnt2_en <= ('1');
        cmp2_ltch1 <= ('1');
          NS_state_mach_rcvr_sm1 <= rst_cnt;

      WHEN rst_cnt =>
        cnt1_en <= ('0');
        cmp1_ltch1 <= ('0');
        cnt2_en <= ('0');
        cmp2_ltch1 <= ('0');
          NS_state_mach_rcvr_sm1 <= rst_cnt;

      WHEN s_bit =>

        IF ((start_pulse = '1')) THEN
          NS_state_mach_rcvr_sm1 <= cnt;
        ELSE
          NS_state_mach_rcvr_sm1 <= s_bit;
        END IF;

      WHEN par1 =>
        par_oe <= ('1');

        IF ((par_det = '0')) THEN
          NS_state_mach_rcvr_sm1 <= ch1;
        ELSIF ((par_det = '1')) THEN
          NS_state_mach_rcvr_sm1 <= rst1;
        ELSE
          NS_state_mach_rcvr_sm1 <= par1;
        END IF;

      WHEN par2 =>
        par_oe <= ('1');

        IF ((par_det = '0')) THEN
          NS_state_mach_rcvr_sm1 <= ch2;
        ELSIF ((par_det = '1')) THEN
          NS_state_mach_rcvr_sm1 <= rst2;
        ELSE
          NS_state_mach_rcvr_sm1 <= par2;
        END IF;

    END CASE;

  END PROCESS; 

sm1_CTL:
  PROCESS (clk_100k, clk_50)
  BEGIN

    IF (clk_100k'event AND clk_100k='1')  THEN 
        IF (clk_50= '1' ) THEN 
            CS_state_mach_rcvr_sm1 <= V_begin;
        ELSE
            CS_state_mach_rcvr_sm1 <= NS_state_mach_rcvr_sm1;
        END IF;
    END IF;

  END PROCESS;


END state_diagram;
--

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;

library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
use IEEE_proposed.mechanical_systems.all;

entity sm_cnt_rcvr is
    port(
        cmp1_ltch1 : out std_logic;
        cmp2_ltch1 : out std_logic;
        s2p_en : out std_logic;
        s2p_rst : out std_logic;
        frm_det : in std_logic;
        par_det : in std_logic;
        clk_100k : in std_logic;
        clk_6k : in std_logic;
        clk_50 : in std_logic;
        start_pulse : in std_logic;
        cnt1_en : out std_logic;
        cnt1_rst : out std_logic;
        cmp1_ltch2 : out std_logic;
        cnt2_en : out std_logic;
        cnt2_rst : out std_logic;
        cmp2_ltch2 : out std_logic;
        da_latch : out std_logic;
        par_oe : out std_logic
    );
end sm_cnt_rcvr;

architecture sm_cnt_rcvr of sm_cnt_rcvr is
    -- Component declarations
    -- Signal declarations
    terminal dly_cnt_a : electrical;
    terminal dly_done_a : electrical;
    terminal ser_cnt_a : electrical;
    terminal ser_done_a : electrical;
    signal XSIG010001 : std_logic;
    signal XSIG010002 : std_logic;
    signal XSIG010145 : std_logic;
    signal XSIG010146 : std_logic;
begin
    -- Signal assignments
    -- Component instances
    XCMP1 : entity work.d2a_bit(ideal)
        port map(
            D => XSIG010001,
            A => ser_cnt_a
        );
    XCMP2 : entity work.d2a_bit(ideal)
        port map(
            D => XSIG010002,
            A => ser_done_a
        );
    bit_cnt3 : entity work.bit_cnt(behavioral)
        generic map(
            count => 2
        )
        port map(
            bit_in => XSIG010145,
            clk => clk_6k,
            dly_out => XSIG010146
        );
    bit_cnt4 : entity work.bit_cnt(behavioral)
        generic map(
            count => 10
        )
        port map(
            bit_in => XSIG010001,
            clk => clk_6k,
            dly_out => XSIG010002
        );
    XCMP8 : entity work.d2a_bit(ideal)
        port map(
            D => XSIG010145,
            A => dly_cnt_a
        );
    XCMP9 : entity work.d2a_bit(ideal)
        port map(
            D => XSIG010146,
            A => dly_done_a
        );
    state_mach_rcvr8 : entity work.state_mach_rcvr
        port map(
            clk_100k => clk_100k,
            clk_50 => clk_50,
            s2p_rst => s2p_rst,
            s2p_en => s2p_en,
            cnt1_en => cnt1_en,
            cnt1_rst => cnt1_rst,
            cmp1_ltch1 => cmp1_ltch1,
            cmp1_ltch2 => cmp1_ltch2,
            cnt2_en => cnt2_en,
            cnt2_rst => cnt2_rst,
            cmp2_ltch1 => cmp2_ltch1,
            cmp2_ltch2 => cmp2_ltch2,
            da_latch => da_latch,
            ser_cnt => XSIG010001,
            ser_done => XSIG010002,
            par_det => par_det,
            frm_det => frm_det,
            clk_6k => clk_6k,
            start_pulse => start_pulse,
            dly_done => XSIG010146,
            dly_cnt => XSIG010145,
            par_oe => par_oe
        );
end sm_cnt_rcvr;
--
-- Copyright Mentor Graphics Corporation 2001
-- Confidential Information Provided Under License Agreement for Internal Use Only

-- level_set.vhd 
-- Set digital output "level" with parameter "logic_val" (default is '1')

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

ENTITY level_set IS

  GENERIC (
    logic_val : std_logic := '1');     

  PORT (
    level : OUT std_logic);

END ENTITY level_set;

-- Simple architecture

ARCHITECTURE ideal OF level_set IS

BEGIN

  level <= logic_val;
  
END ARCHITECTURE ideal;

--

-- Serial to parallel data converter

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
use IEEE_proposed.mechanical_systems.all;

entity ser2par is
port
(
      par_out : inout std_logic_vector(0 to 11) := "ZZZZZZZZZZZZ";
      clk : in std_logic ;
      load_en : in std_logic ;
      ser_in : in std_logic ;
      reset : in std_logic 
);

begin

end ser2par;

architecture a1 of ser2par is
BEGIN 
	sr_sm: PROCESS (load_en, clk, reset, ser_in)	
	BEGIN
		if (reset = '1' and load_en = '1') then
			par_out <= "000000000000";  -- Reset the parallel data out
			
		elsif (clk'event and clk = '1') then
			if (load_en ='1') then 
				
				-- The register will shift when load is enabled 
				-- and will shift at rising edge of clock
				
				par_out(0) <= ser_in; -- Input data shifts into bit 0
				par_out(1) <= par_out(0);
				par_out(2) <= par_out(1);
				par_out(3) <= par_out(2);
				par_out(4) <= par_out(3);
				par_out(5) <= par_out(4);
				par_out(6) <= par_out(5);
				par_out(7) <= par_out(6);
				par_out(8) <= par_out(7);
				par_out(9) <= par_out(8);
				par_out(10) <= par_out(9);
				par_out(11) <= par_out(10); 

			else
			      -- The otput data will not change 
				-- if load_en is not enabled		
		         	par_out <= "ZZZZZZZZZZZZ";			
 			end if;
		end if;			
	END PROCESS;
end;
--

-- This model ouputs a '1' when a specific bit pattern is encountered
-- Otherwise, it outputs a zero

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
use IEEE_proposed.mechanical_systems.all;

entity frame_det is
port
(
	  bus_in : in std_logic_vector (0 to 11);
	  clk : in std_logic;
	  frm_bit : out std_logic := '0' -- Initialize output to zero
	  );

end entity frame_det;

architecture simple of frame_det is
begin
	enbl: PROCESS (bus_in, clk) -- Sensitivity list
  		BEGIN 
		if bus_in = "010101010101" then -- This is the pre-defined bit pattern
			if clk'event AND clk = '0' then -- Output updated synchronously
				frm_bit <= '1';
			end if;
		else frm_bit <= '0';
		end if;
  	END PROCESS;
end architecture simple;

--

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;

library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
use IEEE_proposed.mechanical_systems.all;

entity parity_det is
    port(
        bus_in : in std_logic_vector(0 to 11);
        par_bit : out std_logic;
        oe : in std_logic
    );
end parity_det;

architecture parity_det of parity_det is
    -- Component declarations
    -- Signal declarations
    signal cdb2vhdl_tmp_1 : std_logic;
    terminal par_bit_a : electrical;
    signal XSIG010010 : std_logic;
    signal XSIG010011 : std_logic;
    signal XSIG010012 : std_logic;
    signal XSIG010013 : std_logic;
    signal XSIG010014 : std_logic;
    signal XSIG010015 : std_logic;
    signal XSIG010016 : std_logic;
    signal XSIG010017 : std_logic;
    signal XSIG010019 : std_logic;
    signal XSIG010057 : std_logic;
begin
    -- Signal assignments
    par_bit <= cdb2vhdl_tmp_1;
    -- Component instances
    XCMP1 : entity work.xor2(ideal)
        port map(
            in1 => bus_in(1),
            in2 => bus_in(2),
            output => XSIG010010
        );
    XCMP2 : entity work.xor2(ideal)
        port map(
            in1 => bus_in(3),
            in2 => bus_in(4),
            output => XSIG010011
        );
    XCMP3 : entity work.xor2(ideal)
        port map(
            in1 => bus_in(5),
            in2 => bus_in(6),
            output => XSIG010012
        );
    XCMP4 : entity work.xor2(ideal)
        port map(
            in1 => bus_in(7),
            in2 => bus_in(8),
            output => XSIG010013
        );
    XCMP5 : entity work.xor2(ideal)
        port map(
            in1 => bus_in(9),
            in2 => bus_in(10),
            output => XSIG010016
        );
    XCMP6 : entity work.xor2(ideal)
        port map(
            in1 => XSIG010010,
            in2 => XSIG010011,
            output => XSIG010014
        );
    XCMP7 : entity work.xor2(ideal)
        port map(
            in1 => XSIG010012,
            in2 => XSIG010013,
            output => XSIG010015
        );
    XCMP8 : entity work.xor2(ideal)
        port map(
            in1 => XSIG010014,
            in2 => XSIG010015,
            output => XSIG010017
        );
    XCMP9 : entity work.xor2(ideal)
        port map(
            in1 => XSIG010017,
            in2 => XSIG010016,
            output => XSIG010019
        );
    XCMP10 : entity work.xor2(ideal)
        port map(
            in1 => XSIG010019,
            in2 => bus_in(0),
            output => XSIG010057
        );
    XCMP11 : entity work.d2a_bit(ideal)
        port map(
            D => cdb2vhdl_tmp_1,
            A => par_bit_a
        );
    XCMP12 : entity work.and2(ideal)
        port map(
            in1 => oe,
            in2 => XSIG010057,
            output => cdb2vhdl_tmp_1
        );
end parity_det;
--

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;

library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
use IEEE_proposed.mechanical_systems.all;

entity TDM_Demux_dbg is
    port(
        s2p_en : in std_logic;
        tdm_in : in std_logic;
        clk_6k : in std_logic;
        s2p_rst : in std_logic;
        par_det : out std_logic;
        frm_det : out std_logic;
        da_latch : in std_logic;
        par_oe : in std_logic;
        data_bus : out std_logic_vector(1 to 10);
        start_bit : out std_logic
    );
end TDM_Demux_dbg;

architecture TDM_Demux_dbg of TDM_Demux_dbg is
    -- Component declarations
    -- Signal declarations
    terminal d2a_out : electrical;
    signal rcvr_bus : std_logic_vector(0 to 11);
begin
    -- Signal assignments
    data_bus(1) <= rcvr_bus(1);
    data_bus(2) <= rcvr_bus(2);
    data_bus(3) <= rcvr_bus(3);
    data_bus(4) <= rcvr_bus(4);
    data_bus(5) <= rcvr_bus(5);
    data_bus(6) <= rcvr_bus(6);
    data_bus(7) <= rcvr_bus(7);
    data_bus(8) <= rcvr_bus(8);
    data_bus(9) <= rcvr_bus(9);
    data_bus(10) <= rcvr_bus(10);
    start_bit <= rcvr_bus(0);
    -- Component instances
    s2p1 : entity work.ser2par(a1)
        port map(
            par_out => rcvr_bus,
            clk => clk_6k,
            load_en => s2p_en,
            ser_in => tdm_in,
            reset => s2p_rst
        );
    frm_det1 : entity work.frame_det(simple)
        port map(
            bus_in => rcvr_bus,
            frm_bit => frm_det,
            clk => clk_6k
        );
    par_det1 : entity work.parity_det
        port map(
            bus_in => rcvr_bus,
            par_bit => par_det,
            oe => par_oe
        );
    XCMP113 : entity work.d2a_nbit(behavioral)
        generic map(
            low_bit => 1,
            high_bit => 10,
            vmax => 4.8
        )
        port map(
            bus_in => rcvr_bus(1 to 10),
            ana_out => d2a_out,
            latch => da_latch
        );
end TDM_Demux_dbg;
--

-- Manchester Decoder with clock recovery using 8x referenced clock

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

entity mdec_rsc is
--	port (	din: in real;                -- real input
	port (	din: in std_logic;           -- real input
		clk16x: in std_logic;	     -- 16x referenced clock
		reset: in std_logic;       -- not reset
		bout: out std_logic := '0';         -- digital output
		clk_out: inout std_logic := '0');   -- recovered clock
end entity mdec_rsc;

architecture bhv of mdec_rsc is
--	signal bhigh:real:= 1.0;   	      -- bit decoding 
--	signal blow:real:= -1.0;	      -- bit decoding
--	signal bnormal:real:=0.0;      	      -- bit decoding
	signal bhigh:std_logic:= '1';   	      -- bit decoding 
	signal blow:std_logic:= '0';	      -- bit decoding
	signal bout1:std_logic;
	signal clk_div:std_logic_vector(3 downto 0):="0000";  -- clock counter
	signal trans:std_logic;              -- transisition trigger
begin
	-- bit decoding
	proc1: process (reset,din,clk16x)
	begin
	if (reset = '1') then
	   bout1 <= 'X';
	elsif (clk16x'event and clk16x = '1') then
	   if (din = bhigh) then
	      bout1 <= '1';
	   elsif (din = blow) then
	      bout1 <= '0';
	   else
	      bout1 <= 'X';
	   end if;
	end if;
	end process;
		
	-- clock counter
	proc2: process (reset, clk16x, clk_div)
	begin
	
	if (reset = '1') then
	   clk_div <= "0000";
	elsif (clk16x'event and clk16x = '1') then
	   clk_div <= clk_div + "0001";
	end if;
	end process;
	
	-- recovered clock
	-- clk_out <= not clk_div(3);  
	clk_out <= clk_div(3);  
	
	-- transition trigger
trans <= ((not clk_div(3)) and (not clk_div(2)) and clk_div(1) and clk_div(0)) or 
	          (clk_div(3) and clk_div(2) and (not clk_div(1)) and (not clk_div(0)));

	-- Manchester decoder
	proc3: process (reset, trans, bout1, clk_out, clk16x)
	begin
	if (reset = '1') then
	   bout <= '0';
	elsif (clk16x'event and clk16x = '1') then
	   if  (trans = '1') then
	      bout <= bout1 XOR clk_out;
	   end if;
	end if;
	end process;
	     
end architecture bhv;	

architecture bhv_8 of mdec_rsc is
--	signal bhigh:real:= 1.0;   	      -- bit decoding 
--	signal blow:real:= -1.0;	      -- bit decoding
--	signal bnormal:real:=0.0;      	      -- bit decoding
	signal bhigh:std_logic:= '1';   	      -- bit decoding 
	signal blow:std_logic:= '0';	      -- bit decoding
	signal bout1:std_logic;
	signal clk_div:std_logic_vector(2 downto 0):="000";  -- clock counter
	signal trans:std_logic;              -- transisition trigger
begin
	-- bit decoding
	proc1: process (reset,din,clk16x)
	begin
	if (reset = '1') then
	   bout1 <= 'X';
	elsif (clk16x'event and clk16x = '1') then
	   if (din = bhigh) then
	      bout1 <= '1';
	   elsif (din = blow) then
	      bout1 <= '0';
	   else
	      bout1 <= 'X';
	   end if;
	end if;
	end process;
		
	-- clock counter
	proc2: process (reset, clk16x, clk_div)
	begin
	
	if (reset = '1') then
	   clk_div <= "000";
	elsif (clk16x'event and clk16x = '1') then
	   clk_div <= clk_div + "001";
	end if;
	end process;
	
	-- recovered clock
	clk_out <= not clk_div(2);  
	
	-- transition trigger
	trans <= ((not clk_div(1)) and clk_div(0)) or (clk_div(1) and (not clk_div(0)));

	-- Manchester decoder
	proc3: process (reset, trans, bout1, clk_out, clk16x)
	begin
	if (reset = '1') then
	   bout <= '0';
	elsif (clk16x'event and clk16x = '1') then
	   if  (trans = '1') then
	      bout <= bout1 XOR clk_out;
	   end if;
	end if;
	end process;
	     
end architecture bhv_8;	
--

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;

library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
use IEEE_proposed.mechanical_systems.all;

entity Decode_PW_Man is
    port(
        terminal power : electrical;
        terminal ch1_pw : electrical;
        terminal ch2_pw : electrical;
        bit_stream_in : in std_logic
    );
end Decode_PW_Man;

architecture Decode_PW_Man of Decode_PW_Man is
    -- Component declarations
    -- Signal declarations
    signal bit_stream_in_mdec : std_logic;
    signal clk16x : std_logic;
    signal clk6k : std_logic;
    signal clk_100k : std_logic;
    signal cmp_bus : std_logic_vector(0 to 11);
    signal cnt1 : std_logic_vector(0 to 11);
    signal cnt2 : std_logic_vector(0 to 11);
    signal mdec_clk : std_logic;
    signal mdec_out : std_logic;
    signal reset : std_logic;
    signal reset_m : std_logic;
    signal XSIG010228 : std_logic;
    signal XSIG010229 : std_logic;
    signal XSIG010256 : std_logic;
    signal XSIG010263 : std_logic;
    signal XSIG010264 : std_logic;
    signal XSIG010266 : std_logic;
    signal XSIG010267 : std_logic;
    signal XSIG010268 : std_logic;
    signal XSIG010320 : std_logic;
    signal XSIG010330 : std_logic;
    signal XSIG010334 : std_logic;
    signal XSIG010339 : std_logic;
    signal XSIG010349 : std_logic;
    signal XSIG010357 : std_logic;
    signal XSIG010371 : std_logic;
    signal XSIG010372 : std_logic;
    signal XSIG010373 : std_logic;
    signal XSIG010383 : std_logic;
    signal XSIG010384 : std_logic;
    signal XSIG010385 : std_logic;
    signal XSIG010386 : std_logic;
    signal XSIG010390 : std_logic;
    signal XSIG010433 : std_logic;
begin
    -- Signal assignments
    bit_stream_in_mdec <= bit_stream_in;
    -- Component instances
    cntr1 : entity work.counter_12
        port map(
            enable => XSIG010384,
            cnt => cnt1,
            reset => XSIG010357,
            clk => XSIG010433
        );
    cntr2 : entity work.counter_12
        port map(
            enable => XSIG010349,
            cnt => cnt2,
            reset => XSIG010385,
            clk => XSIG010320
        );
    cmp1 : entity work.dig_cmp(simple)
        port map(
            in1 => cnt1,
            eq => XSIG010371,
            clk => XSIG010433,
            in2 => cmp_bus,
            cmp => XSIG010384,
            latch_in1 => XSIG010256,
            latch_in2 => XSIG010383
        );
    cmp2 : entity work.dig_cmp(simple)
        port map(
            in1 => cnt2,
            eq => XSIG010372,
            clk => XSIG010320,
            in2 => cmp_bus,
            cmp => XSIG010349,
            latch_in1 => XSIG010263,
            latch_in2 => XSIG010264
        );
    XCMP109 : entity work.resistor(ideal)
        generic map(
            res => 1000000.0
        )
        port map(
            p1 => power,
            p2 => ELECTRICAL_REF
        );
    clk_1M2 : entity work.clock_en(ideal)
        generic map(
            pw => 500 ns
        )
        port map(
            CLOCK_OUT => XSIG010320,
            enable => XSIG010349
        );
    clk_1M1 : entity work.clock_en(ideal)
        generic map(
            pw => 500 ns
        )
        port map(
            CLOCK_OUT => XSIG010433,
            enable => XSIG010384
        );
    XCMP134 : entity work.d2a_bit(ideal)
        port map(
            D => XSIG010371,
            A => ch1_pw
        );
    XCMP135 : entity work.d2a_bit(ideal)
        port map(
            D => XSIG010372,
            A => ch2_pw
        );
    XCMP137 : entity work.SR_FF(simple)
        port map(
            S => XSIG010330,
            R => XSIG010334,
            Q => XSIG010349
        );
    XCMP138 : entity work.inverter(ideal)
        port map(
            input => XSIG010372,
            output => XSIG010334
        );
    XCMP139 : entity work.SR_FF(simple)
        port map(
            S => XSIG010373,
            R => XSIG010339,
            Q => XSIG010384
        );
    XCMP140 : entity work.inverter(ideal)
        port map(
            input => XSIG010371,
            output => XSIG010339
        );
    rc_clk2 : entity work.rc_clk
        port map(
            clk_50 => reset,
            clk_6K => clk6k,
            clk_100k => clk_100k
        );
    sm_rcvr1 : entity work.sm_cnt_rcvr
        port map(
            cnt1_en => XSIG010373,
            cmp1_ltch1 => XSIG010256,
            cnt2_rst => XSIG010385,
            clk_100k => clk_100k,
            cnt1_rst => XSIG010357,
            cnt2_en => XSIG010330,
            cmp2_ltch1 => XSIG010263,
            frm_det => XSIG010229,
            par_det => XSIG010228,
            s2p_en => XSIG010266,
            s2p_rst => XSIG010267,
            clk_6k => mdec_clk,
            clk_50 => reset,
            da_latch => XSIG010268,
            cmp1_ltch2 => XSIG010383,
            cmp2_ltch2 => XSIG010264,
            start_pulse => XSIG010390,
            par_oe => XSIG010386
        );
    XCMP155 : entity work.level_set(ideal)
        generic map(
            logic_val => '0'
        )
        port map(
            level => cmp_bus(11)
        );
    XCMP157 : entity work.TDM_Demux_dbg
        port map(
            data_bus => cmp_bus(0 to 9),
            tdm_in => mdec_out,
            clk_6k => mdec_clk,
            s2p_en => XSIG010266,
            s2p_rst => XSIG010267,
            da_latch => XSIG010268,
            frm_det => XSIG010229,
            par_det => XSIG010228,
            par_oe => XSIG010386,
            start_bit => XSIG010390
        );
    XCMP172 : entity work.level_set(ideal)
        generic map(
            logic_val => '1'
        )
        port map(
            level => cmp_bus(10)
        );
    clock1 : entity work.clock(ideal)
        generic map(
            period => 9.375 us
        )
        port map(
            CLK_OUT => clk16x
        );
    mdec_rsc7 : entity work.mdec_rsc(bhv)
        port map(
            din => bit_stream_in_mdec,
            clk16x => clk16x,
            reset => reset_m,
            bout => mdec_out,
            clk_out => mdec_clk
        );
    XCMP181 : entity work.clock_duty(ideal)
        generic map(
            off_time => 19.98 sec
        )
        port map(
            CLOCK_OUT => reset_m
        );
end Decode_PW_Man;
--

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;

library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
use IEEE_proposed.mechanical_systems.all;

entity tb_CS5_CC_Rudder is
end tb_CS5_CC_Rudder;

architecture TB_CS5_CC_Rudder of tb_CS5_CC_Rudder is
    -- Component declarations
    -- Signal declarations
    terminal gear_out : rotational;
    terminal link_in : translational;
    terminal link_out : translational;
    terminal pot_fb : electrical;
    signal rf_in : std_logic;
    signal rf_out : std_logic;
    terminal rudder : rotational;
    terminal rudder_ana : electrical;
    terminal rudder_cmd : electrical;
    terminal rudder_mtr_in : electrical;
    terminal rudder_mtr_out : rotational_v;
    terminal rudder_pw : electrical;
    terminal rudder_servo_in : electrical;
    terminal throttle_ana : electrical;
    terminal throttle_cmd : electrical;
    terminal throttle_pw : electrical;
    terminal XSIG010013 : electrical;
begin
    -- Signal assignments
    -- Component instances
    rudder_servo1 : entity work.rudder_servo
        port map(
            servo_out => rudder_mtr_in,
            servo_in => rudder_servo_in,
            pos_fb => pot_fb
        );
    gear1 : entity work.gear_rv_r(ideal)
        generic map(
            ratio => 0.01
        )
        port map(
            rotv1 => rudder_mtr_out,
            rot2 => gear_out
        );
    potentiometer : entity work.rot2v(bhv)
        generic map(
            k => 1.0
        )
        port map(
            output => pot_fb,
            input => gear_out
        );
    g_horn : entity work.horn_r2t(bhv)
        port map(
            theta => gear_out,
            pos => link_in
        );
    r_horn : entity work.horn_t2r(bhv)
        port map(
            theta => rudder,
            pos => link_out
        );
    \linkage\ : entity work.tran_linkage(a1)
        port map(
            p2 => link_out,
            p1 => link_in
        );
    rudder_1 : entity work.rudder(bhv)
        generic map(
            k => 0.2
        )
        port map(
            rot => rudder
        );
    XCMP6 : entity work.v_constant(ideal)
        generic map(
            level => 5.0
        )
        port map(
            pos => XSIG010013,
            neg => ELECTRICAL_REF
        );
    t_stick : entity work.stick(ideal)
        generic map(
            offset => 2.397,
            phase => 0.0,
            amplitude => 2.397,
            freq => 1.0
        )
        port map(
            v_out => throttle_cmd
        );
    r_stick : entity work.stick(ideal)
        generic map(
            freq => 1.0,
            amplitude => 2.397,
            phase => 270.0,
            offset => 2.397
        )
        port map(
            v_out => rudder_cmd
        );
    RF : entity work.rf_xmtr_rcvr(behavioral)
        port map(
            tdm_in => rf_in,
            tdm_out => rf_out
        );
    Digitize_Encode1 : entity work.Digitize_Encode_Man
        port map(
            ch2_in => rudder_cmd,
            ch1_in => throttle_cmd,
            tdm_out => rf_in
        );
    filter : entity work.lpf_2_e(simple)
        generic map(
            f2 => 10.0,
            f1 => 10.0
        )
        port map(
            input => rudder_ana,
            output => rudder_servo_in
        );
    t_pw2ana : entity work.pw2ana
        port map(
            ana_out => throttle_ana,
            pw_in => throttle_pw
        );
    r_pw2ana : entity work.pw2ana
        port map(
            ana_out => rudder_ana,
            pw_in => rudder_pw
        );
    motor2 : entity work.DC_Motor(basic)
        generic map(
            r_wind => 2.2,
            kt => 3.43e-3,
            l => 2.03e-3,
            d => 5.63e-6,
            j => 168.0e-9
        )
        port map(
            p1 => rudder_mtr_in,
            p2 => ELECTRICAL_REF,
            shaft_rotv => rudder_mtr_out
        );
    stop3 : entity work.stop_r(ideal)
        generic map(
            k_stop => 1.0e6,
            ang_max => 1.05,
            ang_min => -1.05,
            damp_stop => 1.0e2
        )
        port map(
            ang1 => gear_out,
            ang2 => ROTATIONAL_REF
        );
    Decode_PW_Man2 : entity work.Decode_PW_Man
        port map(
            bit_stream_in => rf_out,
            ch2_pw => rudder_pw,
            ch1_pw => throttle_pw,
            power => XSIG010013
        );
end TB_CS5_CC_Rudder;
--













