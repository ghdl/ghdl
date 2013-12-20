
-- Copyright (C) 2001 Bill Billowitch.

-- Some of the work to develop this test suite was done with Air Force
-- support.  The Air Force and Bill Billowitch assume no
-- responsibilities for this software.

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
-- $Id: tc1786.vhd,v 1.2 2001-10-26 16:30:12 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity c09s06b00x00p04n08i01786ent_a is
  port ( signal clk : in bit;
         signal i_bus : in bit_vector(0 to 7);
         signal o_bus : out bit_vector(0 to 7)
         );
end c09s06b00x00p04n08i01786ent_a;

ENTITY c09s06b00x00p04n08i01786ent IS
  port ( signal clock : in bit;
         signal in_bus : in bit_vector(0 to 7);
         signal out_bus : out bit_vector(0 to 7)
         );
END c09s06b00x00p04n08i01786ent;

ARCHITECTURE c09s06b00x00p04n08i01786arch OF c09s06b00x00p04n08i01786ent IS
  component c09s06b00x00p04n08i01786ent_a
    port ( signal clk : in bit;
           signal i_bus : in bit_vector(0 to 7);
           signal o_bus : out bit_vector(0 to 7)
           );
  end component; -- Test

BEGIN
  err : c09s06b00x00p04n08i01786ent_a
    port map ( i_bus => in_bus,
               i_bus => in_bus,
               o_bus => out_bus
               );

  assert FALSE 
    report "***FAILED TEST: c09s06b00x00p04n08i01786 - Each local port must be associated exactly once."
    severity ERROR;

END c09s06b00x00p04n08i01786arch;
