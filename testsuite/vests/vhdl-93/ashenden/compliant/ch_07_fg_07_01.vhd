
-- Copyright (C) 1996 Morgan Kaufmann Publishers, Inc

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

-- ---------------------------------------------------------------------
--
-- $Id: ch_07_fg_07_01.vhd,v 1.1.1.1 2001-08-22 18:20:48 paw Exp $
-- $Revision: 1.1.1.1 $
--
-- ---------------------------------------------------------------------

entity fg_07_01 is
end entity fg_07_01;



architecture test of fg_07_01 is

  shared variable average : real := 0.0;
  type sample_array is array (positive range <>) of real;
  constant samples : sample_array :=
    ( 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0 );

  -- code from book

  procedure average_samples is
    variable total : real := 0.0;
  begin
    assert samples'length > 0 severity failure;
    for index in samples'range loop
      total := total + samples(index);
    end loop;
    average := total / real(samples'length);
  end procedure average_samples;

  -- end code from book

begin

  -- code from book (in text)

  average_samples;

  -- end code from book

end architecture test;
