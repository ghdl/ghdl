
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

-- not in book

library ieee_proposed;  use ieee_proposed.electrical_systems.all;

entity resistor is
  generic ( res : resistance );
  port ( terminal p1, p2 : electrical );
end entity resistor;

architecture ideal of resistor is
  quantity v across i through p1 to p2;
begin
  v == i * res;
end architecture ideal;


library ieee_proposed;  use ieee_proposed.electrical_systems.all;

entity capacitor is
  generic ( cap : resistance );
  port ( terminal p1, p2 : electrical );
end entity capacitor;

architecture ideal of capacitor is
  quantity v across i through p1 to p2;
begin
  i == cap * v'dot;
end architecture ideal;

-- end not in book


library ieee;  use ieee.math_real.all;
library ieee_proposed;  use ieee_proposed.electrical_systems.all;

entity lowpass is
  generic ( gain : real := 1.0;	      -- gain for 'dot, 'ltf, and 'ztf
            fp : real := 10.0;	      -- pole in Hz for 'dot, 'ltf, and 'ztf
            Fsmp : real := 10.0e3 );  -- sample frequency for ztf
  port ( terminal input: electrical;
         terminal output: electrical );
end entity lowpass;

----------------------------------------------------------------

architecture RC of lowpass is

  constant cap : real := 1.0e-6;
  constant res : real := 1.0 / (math_2_pi * cap * fp);

begin

  assert false
    report "gain is ignored in architecture RC" severity note;
  assert false
    report "Fsmp is not used in architecture RC" severity note;

  R : entity work.resistor(ideal)
    generic map( res => res )
    port map( p1 => input, p2 => output );

  C : entity work.capacitor(ideal)
    generic map( cap => cap )
    port map( p1 => output, p2 => electrical_ref );

end architecture RC;

----------------------------------------------------------------

architecture dot of lowpass is

  quantity vin across input to electrical_ref;
  quantity vout across iout through output to electrical_ref;
  constant wp : real := fp * math_2_pi;  -- pole in rad/s
  constant tp : real := 1.0 / wp;        -- time constant

begin

  assert false
    report "Fsmp is not used in architecture dot" severity note;

  vin == (vout + tp * vout'dot) / gain;

end architecture dot;

----------------------------------------------------------------

architecture ltf of lowpass is

  quantity vin across input to electrical_ref;
  quantity vout across iout through output to electrical_ref;
  constant wp : real := fp * math_2_pi;     -- pole in rad/s
  constant num : real_vector := (0 => wp); 
  constant den : real_vector := (wp, 1.0);

begin

  assert false
    report "Fsmp is not used in architecture ltf" severity note;

  vout == gain*vin'ltf(num, den);

end architecture ltf;

----------------------------------------------------------------

architecture z_minus_1 of lowpass is

  quantity vin across input to electrical_ref;
  quantity vout across iout through output to electrical_ref;
  quantity vin_sampled : real;		  -- sampled input 
  quantity vin_zm1, vout_zm1 : real; 	  -- z**-1 
  constant Tsmp : real := 1.0 / Fsmp;	  -- sample period
  constant wp : real := fp * math_2_pi;   -- pole in rad/s
  constant n0 : real := Tsmp * wp;    	  -- z0 numerator coefficient
  constant n1 : real := Tsmp * wp;    	  -- z-1 numerator coefficient
  constant d0 : real := Tsmp * wp + 2.0;  -- z0 denominator coefficient
  constant d1 : real := Tsmp * wp - 2.0;  -- z-1 denominator coefficient

begin

  vin_sampled  == gain*vin'zoh(Tsmp);

  vin_zm1  == vin_sampled'delayed(Tsmp);

  vout_zm1 == vout'delayed(Tsmp);

  vout == vin_sampled * n0 / d0 + n1 * vin_zm1 / d0 - d1 * vout_zm1 / d0;

end z_minus_1;

----------------------------------------------------------------

architecture ztf of lowpass is
  
  quantity vin across input to electrical_ref;
  quantity vout across iout through output to electrical_ref;
  constant Tsmp : real := 1.0 / Fsmp;	  -- sample period
  constant wp : real := fp * math_2_pi;	  -- pole in rad/s
  constant n0 : real := Tsmp * wp; 	  -- z0 numerator coefficient
  constant n1 : real := Tsmp * wp; 	  -- z-1 numerator coefficient
  constant d0 : real := Tsmp * wp + 2.0;  -- z0 denominator coefficient
  constant d1 : real := Tsmp * wp - 2.0;  -- z-1 denominator coefficient
  constant num : real_vector := (n0, n1); 
  constant den : real_vector := (d0, d1);
  
begin
  
  vout == gain*vin'ztf(num, den, Tsmp);
  
end ztf;
