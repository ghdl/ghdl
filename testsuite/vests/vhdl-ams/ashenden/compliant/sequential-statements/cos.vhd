
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

entity cos is
  port ( theta : in real;  result : out real );
end entity cos;

--------------------------------------------------

architecture series of cos is
begin

  summation : process (theta) is
    variable sum, term : real;
    variable n : natural;
  begin
    sum := 1.0;
    term := 1.0;
    n := 0;
    while abs term > abs (sum / 1.0E6) loop
      n := n + 2;
      term := (-term) * theta**2 / real(((n-1) * n));
      sum := sum + term;
    end loop;
    result <= sum;
  end process summation;

end architecture series;


architecture fixed_length_series of cos is
begin

  summation : process (theta) is
    variable sum, term : real;
  begin
    sum := 1.0;
    term := 1.0;
    for n in 1 to 9 loop
      term := (-term) * theta**2 / real(((2*n-1) * 2*n));
      sum := sum + term;
    end loop;
    result <= sum;
  end process summation;

end architecture fixed_length_series;
