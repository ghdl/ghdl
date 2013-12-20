
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

library ieee; use ieee.math_real.all;

package pwl_functions is

  function pwl_dim1_extrap ( x : in real;  xdata, ydata : in real_vector ) 
	return real;

  function interpolate (x,y2,y1,x2,x1 : in real) 
	return real;
  
  function extrapolate (x,y2,y1,x2,x1 : in real) 
	return real;
  
end package pwl_functions;


package body pwl_functions is

  -- code from book  

  function pwl_dim1_extrap ( x : in real;  xdata, ydata : in real_vector ) 
	                   return real is

    variable xvalue, yvalue, m : real;
    variable start, fin, mid: integer;

  begin
    if x <= xdata(0) then
      yvalue := extrapolate ( x, ydata(1), ydata(0), xdata(1), xdata(0) );
      return yvalue;
    end if;

    if x >= xdata(xdata'right) then
      yvalue := extrapolate( x, ydata(ydata'right), ydata(ydata'right - 1),
                                xdata(xdata'right), xdata(xdata'right - 1) );
      return yvalue;
    end if;

    start := 0;
    fin := xdata'right;
    while start <= fin loop
      mid := (start + fin) / 2; 
      if xdata(mid) < x then 
        start := mid + 1;
      else
        fin := mid - 1;
      end if;  
    end loop; 
    if xdata(mid) > x then 
      mid := mid - 1; 
    end if; 
    yvalue := interpolate( x, ydata(mid + 1), ydata(mid),
                              xdata(mid + 1), xdata(mid) );    
    return yvalue;
  end function pwl_dim1_extrap;

  -- end code from book

  function interpolate (x,y2,y1,x2,x1 : in real) 
    return real is 
    variable m, yvalue : real;
  begin
    assert (x1 /= x2)
      report "interpolate: x1 cannot be equal to x2"
      severity error;
    assert (x >= x1) and (x <= x2) 
      report "interpolate: x must be between x1 and x2, inclusively "
      severity error;
    m := (y2 - y1)/(x2 - x1);
    yvalue := y1 + m*(x - x1);
    return yvalue;
  end function interpolate;
	
  function extrapolate (x,y2,y1,x2,x1 : in real) 
    return real is 
    variable m, yvalue : real;
  begin
    assert (x1 /= x2)
      report "extrapolate: x1 cannot be equal to x2"
      severity error;
    assert (x <= x1) or (x >= x2) 
      report "extrapolate: x is within x1, x2 bounds; interpolation will be performed"
      severity warning;
    m := (y2 - y1)/(x2 - x1);
    yvalue := y1 + m*(x - x1);
    return yvalue;
  end function extrapolate;

end package body pwl_functions;
