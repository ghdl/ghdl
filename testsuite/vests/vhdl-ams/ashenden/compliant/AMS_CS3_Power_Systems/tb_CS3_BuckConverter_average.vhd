
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

architecture ideal2 of inductor is

-- Declare Branch Quantities
  quantity v across i through p1 to p2;

begin

  if domain = quiescent_domain and i_ic /= real'low use
    i == i_ic;
  else
    v   == ind * i'dot;                   -- characteristic equation
  end use;

end architecture ideal2;
-------------------------------------------------------------------------------
-- Copyright (c) 2001 Mentor Graphics Corporation
-------------------------------------------------------------------------------
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

library ieee_proposed;  use ieee_proposed.electrical_systems.all;
entity capacitor is
	generic ( cap : capacitance;
				r_esr : resistance := 0.0;
				v_ic : voltage := real'low );
		port ( terminal p1, p2 : electrical );
end entity capacitor;

architecture esr of capacitor is
  quantity v across i through p1 to p2;
  quantity vc : voltage;      -- Internal voltage across capacitor
begin
  if domain = quiescent_domain and v_ic /= real'low use
    vc == v_ic;
    i == 0.0;
  else
    vc == v - (i * r_esr); 
    i == cap * vc'dot;
  end use;
end architecture esr;

-------------------------------------------------------------------------------
-- Copyright (c) 2001 Mentor Graphics Corporation
-------------------------------------------------------------------------------
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
-- File       : v_constant.vhd
-- Author     : Mentor Graphics
-- Created    : 2001/06/16
-- Last update: 2001/07/03
-------------------------------------------------------------------------------
-- Description: Constant Voltage Source
--              Includes Frequency Domain settings
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version     Author              Description
-- 2001/06/16  1.0         Mentor Graphics     Created    
-------------------------------------------------------------------------------

library IEEE;
use IEEE.MATH_REAL.all;
-- Use proposed IEEE natures and packages
library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;

entity v_constant is

  generic (
    level    : voltage;                 -- Constant voltage value [Volts]
    ac_mag   : voltage := 1.0;          -- AC magnitude [Volts]
    ac_phase : real    := 0.0);         -- AC phase [Degrees]

  port (
    terminal pos, neg : electrical);

end entity v_constant;

-------------------------------------------------------------------------------
-- Ideal Architecture (I = constant)
-------------------------------------------------------------------------------
architecture ideal of v_constant is

-- Declare Branch Quantities
  quantity v across i through pos to neg;
-- Declare quantity in frequency domain for AC analysis  
  quantity ac_spec : real spectrum ac_mag, math_2_pi*ac_phase/360.0;

begin

  if domain = quiescent_domain or domain = time_domain use
    v == level;
  else
    v   == ac_spec;      -- used for Frequency (AC) analysis
  end use;

end architecture ideal;

-------------------------------------------------------------------------------
-- Copyright (c) 2001 Mentor Graphics Corporation
-------------------------------------------------------------------------------
--

library IEEE;
use IEEE.std_logic_1164.all;

-- Use proposed IEEE natures and packages
library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;

entity sw_LoopCtrl_wa is
  generic (r_open     : resistance := 1.0e6;
           r_closed   : resistance := 1.0e-3;
	     sw_state   : integer    := 1);       
  port (terminal c, p1, p2 : electrical);

end entity sw_LoopCtrl_wa;

architecture ideal of sw_LoopCtrl_wa is 
  quantity v1 across i1 through c to p1;
  quantity v2 across i2 through c to p2;
  quantity r1, r2 : resistance;  
begin
    if (sw_state = 2) use
      r1 == r_open;
      r2 == r_closed;
    else
      r1 == r_closed;
      r2 == r_open; 
    end use;

  v1 == r1*i1;
  v2 == r2*i2;
end architecture ideal;


library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;

entity pwl_load_wa is
  generic (
    load_enable: string := "yes";
    res_init : resistance;
    res1 : resistance;                  
    t1 : time;
    res2 : resistance;
    t2 : time);
  port (terminal p1, p2 : electrical);
end entity pwl_load_wa;

architecture ideal of pwl_load_wa is
  quantity v across i through p1 to p2;
  signal res_signal : resistance := res_init;
begin

  if load_enable = "yes" use
    if domain = quiescent_domain or domain = frequency_domain use
       v == i*res_init; 
    else 
	   v == i*res_signal'ramp(1.0e-6, 1.0e-6);
	end use;
  else 
    i == 0.0;
  end use;

 -- purpose: Create Events to change resistance at specified times
 -- type   : combinational
 -- inputs :
 -- outputs: res
CreateEvent: process is
 begin  -- process CreateEvent
   wait for t1;
   res_signal <= res1;
   wait for (t2-t1);
   res_signal <= res2;
   wait;
 end process CreateEvent; 

end architecture ideal;

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
    v   == ac_spec;    -- used for Frequency (AC) analysis
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
library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
entity buck_sw is  
  generic (
	Vd		: voltage := 0.7;		-- Diode Voltage
	Vramp		: voltage := 2.5);	-- P-P amplitude of ramp voltage
  port (terminal input, output, ref, ctrl: electrical);
end entity buck_sw;

architecture average of buck_sw is
  quantity Vout across Iout through output to ref;
  quantity Vin across input to ref;
  quantity Vctrl across ctrl to ref;
begin  
   Vout + Vd == Vctrl * Vin / Vramp;    -- Averaged equation
end architecture average;

library IEEE;
library IEEE_proposed;
use ieee.math_real.all;
use IEEE_proposed.electrical_systems.all;

entity comp_2p2z is
  generic (
    gain		: real := 100.0;	-- High DC gain for good load regulation       
    fp1		: real := 7.5e3;	-- Pole location to achieve crossover frequency
    fp2		: real := 531.0e3;-- Pole location to cancel effect of ESR 
    fz1		: real := 403.0;	-- Zero locations to cancel LC filter poles
    fz2		: real := 403.0);
  port (terminal input, output, ref : electrical);
end entity comp_2p2z;

architecture ltf of comp_2p2z is
  quantity vin across input to ref;
  quantity vout across iout through output to ref;
  constant wp1	: real        := math_2_pi*fp1;  -- Pole freq (in radians)
  constant wp2	: real        := math_2_pi*fp2;
  constant wz1	: real        := math_2_pi*fz1;  -- Zero freq (in radians)
  constant wz2	: real        := math_2_pi*fz2;
  constant num	: real_vector	:= (1.0, (wz1+wz2)/(wz1*wz2), 1.0/(wz1*wz2));
  constant den	: real_vector	:= (1.0e-9, 1.0, (wp1+wp2)/(wp1*wp2), 1.0/(wp1*wp2));
begin  
  vout == -1.0*gain*vin'ltf(num, den);
end architecture ltf;

library IEEE;
use IEEE.std_logic_1164.all;

library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
use IEEE_proposed.mechanical_systems.all;

entity TB_CS3_BuckConverter_average is
end TB_CS3_BuckConverter_average;

architecture TB_CS3_BuckConverter_average of TB_CS3_BuckConverter_average is
    -- Component declarations
    -- Signal declarations
    terminal vcomp_out : electrical;
    terminal vctrl : electrical;
    terminal vctrl_init : electrical;
    terminal vin : electrical;
    terminal vmid : electrical;
    terminal vout : electrical;
    terminal vref : electrical;
begin
    -- Signal assignments
    -- Component instances
    L1 : entity work.inductor(ideal)
        generic map(
            ind => 6.5e-3
        )
        port map(
            p1 => vmid,
            p2 => vout
        );
    C1 : entity work.capacitor(ESR)
        generic map(
            cap => 6.0e-6,
            r_esr => 50.0e-3
        )
        port map(
            p1 => vout,
            p2 => ELECTRICAL_REF
        );
    Vctrl_1 : entity work.v_constant(ideal)
        generic map(
            level => 0.327
        )
        port map(
            pos => vctrl_init,
            neg => ELECTRICAL_REF
        );
    Vref_1 : entity work.v_constant(ideal)
        generic map(
            level => 4.8
        )
        port map(
            pos => vref,
            neg => ELECTRICAL_REF
        );
    sw2 : entity work.sw_LoopCtrl_wa(ideal)
        generic map(
            sw_state => 1
        )
        port map(
            p2 => vctrl_init,
            c => vctrl,
            p1 => vcomp_out
        );
    Electrical_Load6 : entity work.pwl_load_wa(ideal)
        generic map(
            t2 => 30 ms,
            res2 => 5.0,
            t1 => 5ms,
            res1 => 1.0,
            res_init => 2.4,
            load_enable => "yes"
        )
        port map(
            p1 => vout,
            p2 => ELECTRICAL_REF
        );
    Vin_1 : entity work.v_pulse(ideal)
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
    buck_sw2 : entity work.buck_sw(average)
        port map(
            ctrl => vctrl,
            input => vin,
            ref => ELECTRICAL_REF,
            output => vmid
        );
    comp_2p2z4 : entity work.comp_2p2z(ltf)
        generic map(
            fz1 => 403.0,
            fz2 => 403.0,
            gain => 100.0
        )
        port map(
            input => vout,
            output => vcomp_out,
            ref => vref
        );
end TB_CS3_BuckConverter_average;
--

