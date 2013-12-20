
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

package stimulus_types is

  constant stimulus_vector_length : positive := 4;

  type stimulus_element is record
      application_time : delay_length;
      pattern : real_vector(0 to stimulus_vector_length - 1);
    end record stimulus_element;

  function stimulus_key ( stimulus : stimulus_element ) return delay_length;

end package stimulus_types;

----------------------------------------------------------------

package body stimulus_types is

  function stimulus_key ( stimulus : stimulus_element ) return delay_length is
  begin
    return stimulus.application_time;
  end function stimulus_key;

end package body stimulus_types;
