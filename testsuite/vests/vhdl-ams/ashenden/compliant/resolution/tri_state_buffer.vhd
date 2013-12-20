
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

use work.MVL4.all;

entity tri_state_buffer is
  port ( a, enable : in MVL4_ulogic;  y : out MVL4_ulogic );
end entity tri_state_buffer;

--------------------------------------------------

architecture behavioral of tri_state_buffer is
begin

  y <= 'Z' when enable = '0' else
       a   when enable = '1' and (a = '0' or a = '1') else
       'X';

end architecture behavioral;
