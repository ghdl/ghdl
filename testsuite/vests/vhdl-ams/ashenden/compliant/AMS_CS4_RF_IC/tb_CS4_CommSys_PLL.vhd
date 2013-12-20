
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
-- File       : VCOAnalog.vhd
-- Author     : Mentor Graphics
-- Created    : 2001/07/11
-- Last update: 2002/05/21
-------------------------------------------------------------------------------
-- Description: Analog Voltage Controlled Oscillator
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version     Author              Description
-- 2001/07/11  1.0         Mentor Graphics     Created    
-------------------------------------------------------------------------------
library IEEE;
use IEEE.math_real.all;
library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;

entity VCOAnalog is
  generic (
    Kv          : real := 100.0e3;   -- VCO Gain [Hz/Volt]
    Fc          : real := 1.0e6;     -- center freq [Hz]
    Vc          : voltage := 2.5;    -- input voltage that gives fc [Volts] 
    Vcmin       : voltage := 0.0;    -- control voltage mininum [Volts]
    Vcmax       : voltage := 5.0;    -- control voltage maximum [Volts]
    Vout_ampl   : voltage := 1.0;    -- amplitude of output [Volts]
    Vout_offset : voltage := 0.0     -- offset voltage of output [Volts]
    );
  port (
    terminal v_inp, v_inm, output : electrical);
end entity VCOAnalog;

-------------------------------------------------------------------------------
-- VCO Equation:
-- Fout = Fc + Kv*Vin
-------------------------------------------------------------------------------
architecture behavioral of VCOAnalog is
  quantity vout across iout through output to electrical_ref;
  quantity vctrl across v_inp to v_inm;
  quantity phi : real;
  quantity vtmp : real;
  constant Kv_w : real := math_2_pi*Kv;    -- convert to (Rad/s)/Volt
  constant wc : real := math_2_pi*Fc;        -- convert freq to Rad/s

begin  -- ARCHITECTURE behavioral

  if vctrl > Vcmax use                  -- test control voltage for limits
    vtmp == Vcmax;
  elsif vctrl < Vcmin use
    vtmp == Vcmin;
  else
    vtmp == vctrl;
  end use;
  									       
  if domain = quiescent_domain use
    phi     == 0.0;
  else
  -- use one of the following equations depending on preference
  --  phi'dot == Fc + Kv*(vtmp-Vc);     -- Calculate output Freq in Rad/s
    phi'dot == wc + Kv_w*(vtmp-Vc);     -- Calculate output Freq in Hz
  end use;

-- Use one of the following equations depending on phi'dot equation above
--vout == Vout_offset + Vout_ampl*cos(math_2_pi*phi);
vout == Vout_offset + Vout_ampl*cos(phi);

end architecture behavioral;

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
-- File       : vLeadLag.vhd
-- Author     : Mentor Graphics
-- Created    : 2001/11/09
-- Last update: 2001/11/27
-------------------------------------------------------------------------------
-- Description: Lead-Lag filter with electrical connections
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version     Author              Description
-- 2001/11/09  1.0         Mentor Graphics     Created    
-------------------------------------------------------------------------------
library ieee;
use ieee.math_real.all;
library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;

entity vLeadLag is

  generic (
    K    : real := 1.0;                 -- gain
    Fp   : real := 20.0e3;              -- pole frequency
    Fz   : real := 1.0e6);              -- zero frequency

  port (
    terminal input, output : electrical);

end entity vLeadLag;

-------------------------------------------------------------------------------
-- Transfer Fucntion:
--
--              1 + (s/wz)
--  H(s) = K * ------------
--              1 + (s/wp)
-- 
-------------------------------------------------------------------------------

architecture behavioral of vLeadLag is

  quantity vin across input to electrical_ref;
  quantity vout across iout through output to electrical_ref;
  constant wp  : real        := math_2_pi*Fp;  -- Pole freq (in radians)
  constant wz  : real        := math_2_pi*Fz;  -- Zero freq (in radians)
  constant num : real_vector := (1.0, 1.0/wz);
  constant den : real_vector := (1.0, 1.0/wp);

begin

  vout == K * vin'ltf(num, den);        -- Laplace transform of input

end architecture behavioral;

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
-- File       : vMult.vhd
-- Author     : Mentor Graphics
-- Created    : 2001/11/09
-- Last update: 2001/11/09
-------------------------------------------------------------------------------
-- Description: Two input Multiplier with electrical connections 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version     Author              Description
-- 2001/11/09  1.0         Mentor Graphics     Created    
-------------------------------------------------------------------------------

library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;

entity vMult is

  generic (K : real := 1.0);            -- Gain

  port (
    terminal in1, in2 : electrical;
    terminal output   : electrical);

end entity vMult;

architecture behavioral of vMult is

  quantity vin1 across in1 to electrical_ref;
  quantity vin2 across in2 to electrical_ref;
  quantity vout across iout through output to electrical_ref;

begin

  vout == k * vin1 * vin2;

end architecture behavioral;

-------------------------------------------------------------------------------
-- Copyright (c) 2001 Mentor Graphics Corporation
-------------------------------------------------------------------------------
--

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;

library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
use IEEE_proposed.mechanical_systems.all;
use IEEE_proposed.fluidic_systems.all;
use IEEE_proposed.thermal_systems.all;
use IEEE_proposed.radiant_systems.all;

entity PLL is
    port(
        terminal lf_out : electrical;
        terminal input : electrical;
        terminal vco_out : electrical
    );
end PLL;

architecture PLL of PLL is
    -- Component declarations
    -- Signal declarations
    terminal pd_out : electrical;
begin
    -- Signal assignments
    -- Component instances
    vco2 : entity work.VCOAnalog(behavioral)
        generic map(
            Fc => 455.0e3,
            Vcmax => 5.0,
            Vcmin => -5.0,
            Vc => 0.0
        )
        port map(
            v_inp => lf_out,
            output => vco_out,
            v_inm => ELECTRICAL_REF
        );
    vLeadLag1 : entity work.vLeadLag(behavioral)
        generic map(
            Fz => 500.0e3
        )
        port map(
            input => pd_out,
            output => lf_out
        );
    vmult1 : entity work.vMult(behavioral)
        port map(
            in1 => input,
            in2 => vco_out,
            output => pd_out
        );
end PLL;
--

-- Model of Binary Frequency Shift Keying (BFSK) modulator
-- with digital input and analog output

library IEEE;
library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
use IEEE.STD_LOGIC_1164.all;
use IEEE.MATH_REAL.all;

entity bfsk is
  generic (
    fc      : real    := 455.0e3;   -- Mean carrier frequency
    delta_f : real    := 5.0e3;     -- Difference between low and high carrier frequency
    amp     : voltage := 1.0;       -- Amplitude of modulated signal
	offset  : voltage := 0.0        -- output offset voltage
    );

  port (
    d_in           : in std_logic;       -- digital input
    terminal a_out :    electrical       -- output terminal 
    );
end entity bfsk;

architecture behavioral of bfsk is

  quantity vout across iout through a_out;      -- output branch
  quantity phi : real;                          -- free quantity for angle in radians
  constant wc : real := math_2_pi*fc;           -- convert fc to rad/s
  constant delta_w : real := math_2_pi*delta_f; -- convert delta_f to rad/s

begin

  if (d_in = '0') use
    phi'dot == wc;               -- set to carrier frequency
  elsif (d_in = '1') use
    phi'dot == wc + delta_w;	 -- set to carrier frequency + delta
  else
    phi'dot == 0.0;
  end use;

  vout == offset + amp*sin(phi); -- create sinusoidal output using phi

end architecture behavioral;

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
-- File       : vLPF_2nd.vhd
-- Author     : Mentor Graphics
-- Created    : 2001/11/27
-- Last update: 2001/11/27
-------------------------------------------------------------------------------
-- Description: 2nd order Lowpass Filter with Electrical connections 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version     Author              Description
-- 2001/11/27  1.0         Mentor Graphics     Created    
-------------------------------------------------------------------------------
library IEEE;
use IEEE.MATH_REAL.all;
library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;

entity vLPF_2nd is
  generic ( K  : real := 1.0;     -- Filter Gain
            Fp : real;            -- Double Pole Frequency [Hz]
            Q  : real := 0.707    -- Quality factor
            );
  port ( terminal input  : electrical; 
         terminal output : electrical   
         );
end entity vLPF_2nd;
-------------------------------------------------------------------------------
-- Transfer Function:
-- 
--                    wp^2
-- Vo(s) = K * --------------------- Vin(s)
--             S^2 + (wp/Q)*s + wp^2
-------------------------------------------------------------------------------
architecture behavioral of vLPF_2nd is
  quantity vin across input;                 
  quantity vout across iout through output; 

  constant wp  : real        := math_2_pi*Fp;        -- Frequency in Radians
  constant num : real_vector := (wp*wp, 0.0, 0.0);   -- Numerator array
  constant den : real_vector := (wp*wp, wp/Q, 1.0);  -- Denominator array

begin

  vout == K * vin'ltf(num, den);        -- Laplace Transform of input

end architecture behavioral;

-------------------------------------------------------------------------------
-- Copyright (c) 2001 Mentor Graphics Corporation
-------------------------------------------------------------------------------

library ieee_proposed;  
use ieee_proposed.electrical_systems.all;

entity MeasFreq is
  generic ( thres : real := 0.0 );    -- threshold crossing
  port ( terminal input : electrical;
         signal f_out : out real := 0.0);
end entity MeasFreq;

architecture ThresDetect of MeasFreq is
  quantity vin across input;
--  signal freq : real := 0.0;           
begin
--  f_out <= freq; 
  detect : process (vin'above(thres)) is
    variable t_old : real := real'low;
  begin
    if vin'above(thres) then
      f_out <= 1.0 / (now - t_old);
      t_old := now;
    end if;
  end process detect;
end ThresDetect;
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
-- File       : a2d_bit.vhd
-- Author     : Mentor Graphics
-- Created    : 2001/06/16
-- Last update: 2001/06/16
-------------------------------------------------------------------------------
-- Description: Ideal one bit A/D converter
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version     Author              Description
-- 2001/06/16  1.0         Mentor Graphics     Created    
-------------------------------------------------------------------------------

library IEEE;
use IEEE.math_real.all;
use IEEE.std_logic_1164.all;

library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;

entity a2d_bit is

  generic (
    thres : real := 2.5);               -- Threshold to determine logic output

  port (
    terminal a :     electrical;        -- analog input
    signal   d : out std_logic);        -- digital (std_logic) output

end entity a2d_bit;

-------------------------------------------------------------------------------
-- Ideal architecture
-- Uses 'above operator to detect threshold crossing
-------------------------------------------------------------------------------
architecture ideal of a2d_bit is

  quantity vin across a;

begin
  
  -- purpose: Detect threshold crossing and assign event on output (d)
  -- type   : combinational
  -- inputs : vin'above(thres)
  -- outputs: pulse_signal  
  process (vin'above(thres)) is
  begin  -- PROCESS
    if vin'above(thres) then
      d <= '1';
    else
      d <= '0';
    end if;
  end process;

end ideal;

-------------------------------------------------------------------------------
-- Copyright (c) 2001 Mentor Graphics Corporation
-------------------------------------------------------------------------------
--

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;

library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
use IEEE_proposed.mechanical_systems.all;
use IEEE_proposed.fluidic_systems.all;
use IEEE_proposed.thermal_systems.all;
use IEEE_proposed.radiant_systems.all;

entity tb_CS4_CommSys_PLL is
end tb_CS4_CommSys_PLL;

architecture TB_CS4_CommSys_PLL of tb_CS4_CommSys_PLL is
    -- Component declarations
    -- Signal declarations
    terminal a_out : electrical;
    signal baseband : std_logic;
    terminal fsk_out : electrical;
    signal fsk_out_f : real;
    terminal lpf_pll_out : electrical;
    terminal vco_out : electrical;
  signal  bitstream : std_logic;
    signal vco_out_f : real;
begin
    -- Signal assignments
    -- Component instances
    pll3 : entity work.PLL
        port map(
            vco_out => vco_out,
            input => fsk_out,
            lf_out => lpf_pll_out
        );
    BFSK4 : entity work.bfsk(behavioral)
        port map(
            d_in => bitstream,
            a_out => fsk_out
        );
    vLPF1 : entity work.vLPF_2nd(behavioral)
        generic map(
            K => 200.0,
            Fp => 50.0e3
        )
        port map(
            input => lpf_pll_out,
            output => a_out
        );
    MeasFreq8 : entity work.MeasFreq(ThresDetect)
        port map(
            input => fsk_out,
            f_out => fsk_out_f
        );
    MeasFreq9 : entity work.MeasFreq(ThresDetect)
        port map(
            input => vco_out,
            f_out => vco_out_f
        );
    a4 : entity work.a2d_bit(ideal)
        port map(
            D => baseband,
            A => a_out
        );
  -- bitstream
    P_bitstream :
    process
    begin
      -- 0.000 
                        wait for 0.000 ns;      bitstream <=  '0';
      -- 50000.000 
                        wait for 50000.000 ns;      bitstream <=  '1';
      -- 100000.000 
                        wait for 50000.000 ns;      bitstream <=  '0';
      -- 150000.000 
                        wait for 50000.000 ns;      bitstream <=  '1';
      -- 200000.000 
                        wait for 50000.000 ns;      bitstream <=  '0';
      -- 300000.000 
                        wait for 100000.000 ns;      bitstream <=  '1';
      -- 501000.000 
                        wait for 201000.000 ns;      bitstream <=  '0';
      -- 550000.000 
                        wait for 49000.000 ns;      bitstream <=  '1';
      -- 600000.000 
                        wait for 50000.000 ns;      bitstream <=  '0';
      wait;
    end process;

end TB_CS4_CommSys_PLL;


