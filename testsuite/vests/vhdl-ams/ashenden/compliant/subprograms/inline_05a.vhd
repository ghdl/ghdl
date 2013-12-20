
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

entity inline_05a is

end entity inline_05a;


architecture test of inline_05a is

  function limited ( value, min, max : real ) return real is
  begin
    if value > max then
      return max;
    elsif value < min then
      return min;
    else
      return value;
    end if;
  end function limited;

  quantity v_in, v_amplified : real;
  constant gain : real := 10.0;
  constant v_neg : real := -10.0;
  constant v_pos : real := 10.0;
  
begin

  -- code from book

  v_amplified == limited ( gain * v_in, v_neg, v_pos );

  -- end code from book

end architecture test;
