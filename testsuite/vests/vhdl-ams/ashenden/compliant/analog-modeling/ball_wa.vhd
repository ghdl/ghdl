
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

ENTITY ball_wa IS 
END ENTITY ball_wa; 

ARCHITECTURE simple OF ball_wa IS 
	QUANTITY v: real; 
	QUANTITY s: real; 
	CONSTANT G: real := 9.81; 
	CONSTANT Air_Res: real := 0.1; 
	SIGNAL damping: real := -0.7; 
	signal v_at_impact : real:= 0.0; 
	signal impact: boolean; 
BEGIN 
	if domain = quiescent_domain use 
		v == 0.0; 
		s == 30.0; 
	elsif impact use 
		v == damping*v_at_impact; 
		s == 0.0; 
	else 
		s'dot == v; 
		v'dot == -G; 
	end use; 
	process begin 
		wait until not s'above(0.0); 
		if v < -1.0e-9 then 
			v_at_impact <= v; 
			impact <= true, false after 1 us; 
		else 
			damping <= 0.0; 
			impact <= true; 
		end if; 
	end process; 
	break on impact; 
END architecture simple; 



