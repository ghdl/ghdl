
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

library ieee_proposed;  use ieee_proposed.electrical_systems.all;

entity CalcBuckParams is

  generic ( Vin : voltage range 1.0 to 50.0 := 42.0;  -- input voltage [volts]
            Vout : voltage := 4.8;		      -- output voltage [volts]
            Vd : voltage := 0.7;		      -- diode voltage [volts]
            Imin : current := 15.0e-3;	              -- min output current [amps]
            Vripple : voltage range 1.0e-6 to 100.0
                        := 100.0e-3 );                -- output voltage ripple [volts]

  port ( quantity Fsw : in real range 1.0 to 1.0e6
                          := 2.0;                     -- switching frequency [Hz]
         quantity Lmin : out inductance;	      -- minimum inductance [henries]
         quantity Cmin : out capacitance );	      -- minimum capacitance [farads]

end entity CalcBuckParams;

----------------------------------------------------------------

architecture behavioral of CalcBuckParams is

  constant D : real := (Vout + Vd) / Vin;  -- duty cycle
  quantity Ts : real;		           -- period
  quantity Ton : real;		           -- on time

begin

    Ts == 1.0 / Fsw;

    Ton == D * Ts;

    Lmin == (Vin - Vout) * Ton / (2.0 * Imin);

    Cmin == (2.0 * Imin) / (8.0 * Fsw * Vripple);

end architecture behavioral;
