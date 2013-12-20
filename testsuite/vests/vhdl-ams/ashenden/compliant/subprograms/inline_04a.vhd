
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

entity inline_04a is

end entity inline_04a;


architecture test of inline_04a is
  
  -- code from book
  
  function vector_multiply ( p : real_vector;  r : real ) return real_vector is 
    variable result : real_vector(p'range);
  begin
    for index in p'range loop
      result(index) := p(index) * r;
    end loop;
    return result;
  end function vector_multiply;
        
  --
  
  quantity scale_factor : real;
  quantity source_position, scaled_position : real_vector(1 to 3);

  -- end code from book
  
begin

  -- code from book
  
  scaled_position == vector_multiply ( source_position, scale_factor );

  -- end code from book
  
end architecture test;
