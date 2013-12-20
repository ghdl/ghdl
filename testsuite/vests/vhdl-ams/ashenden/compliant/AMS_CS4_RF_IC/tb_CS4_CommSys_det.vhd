
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
-- File       : capacitor.vhd
-- Author     : Mentor Graphics
-- Created    : 2001/06/16
-- Last update: 2002/05/21
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
    v_ic            : real := real'low);  -- Initial voltage (activated by
                                          -- IF statement below)
  
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
-- File       : resistor.vhd
-- Author     : Mentor Graphics
-- Created    : 2001/06/16
-- Last update: 2001/06/16
-------------------------------------------------------------------------------
-- Description: Electrical Resistor
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version     Author              Description
-- 2001/06/16  1.0         Mentor Graphics     Created    
-------------------------------------------------------------------------------

-- Use proposed IEEE natures and packages
library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;

entity resistor is

  generic (
    res : resistance);                  -- resistance (no initial value)

  port (
    terminal p1, p2 : electrical);

end entity resistor;

-------------------------------------------------------------------------------
-- Ideal Architecture (V = I*R)
-------------------------------------------------------------------------------
architecture ideal of resistor is

  quantity v across i through p1 to p2;

begin

-- Characteristic equation
  v == i*res;

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
-- File       : diode.vhd
-- Author     : Mentor Graphics
-- Created    : 2001/06/16
-- Last update: 2001/11/07
-------------------------------------------------------------------------------
-- Description: Diode model with ideal architecture
--              Currently no Generics due to bug in DV 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version     Author              Description
-- 2001/06/16  1.0         Mentor Graphics     Created
-- 2001/11/07  1.1         Mentor Graphics     Added limit_exp function
-------------------------------------------------------------------------------

library IEEE;
use IEEE.math_real.all;

-- Use proposed IEEE natures and packages
library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;
-- energy_systems package needed for Boltzmann constant (K = Joules/Kelvin)
use IEEE_proposed.energy_systems.all;

entity diode is

  port (
    terminal p, n : electrical);

end entity diode;

-------------------------------------------------------------------------------
-- Ideal Architecture: i = is*(exp(v/vt) - 1)
-------------------------------------------------------------------------------
architecture ideal of diode is
  
-- Declare internal quanties and constants
  quantity v across i through p to n;
  constant isat  : current := 1.0e-14;        -- Saturation current [Amps]
  constant TempC : real    := 27.0;           -- Ambient Temperature [Degrees]
  constant TempK : real    := 273.0 + TempC;  -- Temperaure [Kelvin] 
  constant vt    : real    := K*TempK/Q;      -- Thermal Voltage

  -- This function is to limit the exponential function to avoid convergence
  -- problems due to numerical overflow. At x=100, it becomes a straight line
  -- with slope matching that at the intercept.  
  function limit_exp( x : real ) return real is
    variable abs_x      : real := abs(x);
    variable result     : real;
  begin
    if abs_x < 100.0 then
      result := exp(abs_x);
    else
      result := exp(100.0) * (abs_x - 99.0);
    end if;
  -- If exponent is negative, set exp(-x) = 1/exp(x)  
    if x < 0.0 then
      result := 1.0 / result;
    end if;
    return result;
  end function limit_exp;
begin  -- ideal architecture

-- Characteristic equation
  i == isat*(limit_exp(v/vt) - 1.0);

end architecture ideal;

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

entity EnvDetect is
    port(
        terminal input : electrical;
        terminal output : electrical
    );
end EnvDetect;

architecture EnvDetect of EnvDetect is
    -- Component declarations
    -- Signal declarations
    terminal XSIG010001 : electrical;
begin
    -- Signal assignments
    -- Component instances
    C1 : entity work.capacitor(ideal)
        generic map(
            cap => 0.1e-6
        )
        port map(
            p1 => XSIG010001,
            p2 => ELECTRICAL_REF
        );
    R1 : entity work.resistor(ideal)
        generic map(
            res => 1.0e3
        )
        port map(
            p1 => XSIG010001,
            p2 => ELECTRICAL_REF
        );
    D4 : entity work.diode(ideal)
        port map(
            p => input,
            n => XSIG010001
        );
    C2 : entity work.capacitor(ideal)
        generic map(
            cap => 6.0e-6
        )
        port map(
            p1 => XSIG010001,
            p2 => output
        );
    R6 : entity work.resistor(ideal)
        generic map(
            res => 1.0e3
        )
        port map(
            p1 => output,
            p2 => ELECTRICAL_REF
        );
end EnvDetect;
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
-- File       : vSum.vhd
-- Author     : Mentor Graphics
-- Created    : 2001/11/09
-- Last update: 2001/11/09
-------------------------------------------------------------------------------
-- Description: Summing junction with electrical connections
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version     Author              Description
-- 2001/11/09  1.0         Mentor Graphics     Created    
-------------------------------------------------------------------------------
library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;

entity vSum is

  generic (
    K1 : real := 1.0;
    K2 : real := -1.0);

  port (
    terminal in1, in2 : electrical;
    terminal output   : electrical);

end entity vSum;

architecture behavioral of vSum is

  quantity vin1 across in1 to electrical_ref;
  quantity vin2 across in2 to electrical_ref;
  quantity vout across iout through output to electrical_ref;

begin

  vout == K1*vin1 + K2*vin2;

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
-- File       : vBPF.vhd
-- Author     : Mentor Graphics
-- Created    : 2001/11/27
-- Last update: 2001/11/27
-------------------------------------------------------------------------------
-- Description: Bandpass Filter with Electrical connections 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version     Author              Description
-- 2001/11/27  1.0         Mentor Graphics     Created    
-------------------------------------------------------------------------------

library IEEE;
use IEEE.MATH_REAL.all;
library IEEE_proposed;
use IEEE_proposed.electrical_systems.all;

entity vBPF is
  generic ( K  : real := 1.0;       -- Filter Gain
            Fc : real;              -- Center Frequency [Hz]
            Q  : real := 0.707      -- Quality factor
            );
  port ( terminal input  : electrical;   
         terminal output : electrical  
         );
end entity vBPF;
-------------------------------------------------------------------------------
-- Transfer Function:
-- 
--                  wc*s
-- Vo(s) = K * --------------------- Vin(s)
--             S^2 + (wc/Q)*s + wc^2
-------------------------------------------------------------------------------
architecture behavioral of vBPF is
  quantity vin across input;                        
  quantity vout across iout through output;          
  
  constant wc  : real        := math_2_pi*Fc;       -- Frequency in Radians
  constant num : real_vector := (0.0, wc);          -- Numerator array
  constant den : real_vector := (wc*wc, wc/Q, 1.0); -- Denominator array

begin

  vout == K * vin'ltf(num, den);     -- Laplace Transform of output

end architecture behavioral;

-------------------------------------------------------------------------------
-- Copyright (c) 2001 Mentor Graphics Corporation
-------------------------------------------------------------------------------
--

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

entity tb_CS4_CommSys_det is
end tb_CS4_CommSys_det;

architecture TB_CS4_CommSys_det of tb_CS4_CommSys_det is
    -- Component declarations
    -- Signal declarations
    signal baseband : std_logic;
    signal bitstream : std_logic;
    terminal bp1_out : electrical;
    terminal bp2_out : electrical;
    terminal ed1_out : electrical;
    terminal ed2_out : electrical;
    terminal fsk_out : electrical;
    signal fsk_out_f : real;
    terminal lna_in : electrical;
    terminal lna_out : electrical;
begin
    -- Signal assignments
    -- Component instances
    EnvDetect1 : entity work.EnvDetect
        port map(
            output => ed1_out,
            input => bp1_out
        );
    EnvDetect2 : entity work.EnvDetect
        port map(
            output => ed2_out,
            input => bp2_out
        );
    BFSK3 : entity work.bfsk(behavioral)
        generic map(
            amp => 5.0
        )
        port map(
            d_in => bitstream,
            a_out => fsk_out
        );
    vsum1 : entity work.vSum(behavioral)
        port map(
            in1 => ed1_out,
            in2 => ed2_out,
            output => lna_in
        );
    vLPF2 : entity work.vLPF_2nd(behavioral)
        generic map(
            Fp => 20.0e3,
            K => 10000.0
        )
        port map(
            input => lna_in,
            output => lna_out
        );
    vBPF2 : entity work.vBPF(behavioral)
        generic map(
            Fc => 455.0e3
        )
        port map(
            input => fsk_out,
            output => bp2_out
        );
    vBPF3 : entity work.vBPF(behavioral)
        generic map(
            Fc => 460.0e3
        )
        port map(
            input => fsk_out,
            output => bp1_out
        );
    MeasFreq6 : entity work.MeasFreq(ThresDetect)
        port map(
            input => fsk_out,
            f_out => fsk_out_f
        );
    a2 : entity work.a2d_bit(ideal)
        generic map(
            thres => 1.0
        )
        port map(
            D => baseband,
            A => lna_out
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

--     KillerProc :
--     process
--     begin
--       wait for 1 ns;
--       lclclkinitwire <= '1';
--       wait;
--     end process;
end TB_CS4_CommSys_det;
 
 

