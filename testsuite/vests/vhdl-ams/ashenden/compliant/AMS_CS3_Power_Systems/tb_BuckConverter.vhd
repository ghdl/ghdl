
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

library ieee;  use ieee.std_logic_1164.all;
library ieee_proposed;  use ieee_proposed.electrical_systems.all;

entity tb_BuckConverter is
  port ( ctrl : std_logic );
end tb_BuckConverter;

----------------------------------------------------------------

architecture tb_BuckConverter of tb_BuckConverter is

  terminal vin : electrical;
  terminal vmid : electrical;
  terminal vout : electrical;
    
begin

  L1 : entity work.inductor(ideal)
    generic map ( ind => 6.5e-3 )
    port map ( p1 => vmid, p2 => vout );
  
  C1 : entity work.capacitor(ideal)
    generic map ( cap => 1.5e-6 )
    port map ( p1 => vout, p2 => electrical_ref );
  
  VinDC : entity work.v_constant(ideal)
    generic map ( level => 42.0 )
    port map ( pos => vin, neg => electrical_ref );
  
  RLoad : entity work.resistor(ideal)
    generic map ( res => 2.4 )
    port map ( p1 => vout, p2 => electrical_ref );
  
  D1 : entity work.diode(ideal)
    port map ( p => electrical_ref, n => vmid );
  
  sw1 : entity work.switch_dig(ideal)
    port map ( sw_state => ctrl, p2 => vmid, p1 => vin );

end architecture tb_BuckConverter;
