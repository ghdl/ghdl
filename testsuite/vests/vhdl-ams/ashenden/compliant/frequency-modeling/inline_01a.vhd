
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
library ieee;  use ieee.math_real.all;

entity inline_01a is

end entity inline_01a;


architecture test of inline_01a is

  function inverse_exp ( x : real ) return real is
  begin
    return 10.0 * exp(-2.0e-6 * x);
  end function inverse_exp;

  -- code from book

  type domain_type is (quiescent_domain, time_domain, frequency_domain);

  --

  quantity spec_source : real spectrum 2.5, math_pi / 2.0;

  --

  function frequency return real;

  --

  quantity source1 : real spectrum inverse_exp(frequency), math_pi / 4.0;

  --

  quantity source2 : real spectrum 5.0, 1.0E-6 * frequency / math_pi;

  -- end code from book

  function frequency return real is
  begin
    return std.standard.frequency;
  end function frequency;

begin
end architecture test;
