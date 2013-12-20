
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


library ieee_proposed;  use ieee_proposed.electrical_systems.all;

entity lowpass is
  port ( terminal input : electrical;
         terminal output : electrical );
end entity lowpass;

----------------------------------------------------------------

architecture RC of lowpass is
begin

  R : entity work.resistor(ideal)
    generic map ( res => 15.9e3 )
    port map ( p1 => input, p2 => output );

  C : entity work.capacitor(ideal)
    generic map ( cap => 1.0e-6 )
    port map ( p1 => output, p2 => electrical_ref );

end architecture RC;
