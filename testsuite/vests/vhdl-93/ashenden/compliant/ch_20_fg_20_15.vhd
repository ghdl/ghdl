
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
-- $Id: ch_20_fg_20_15.vhd,v 1.2 2001-10-26 16:29:36 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package cell_attributes is

  type length is range 0 to integer'high
    units nm;
          um = 1000 nm;
          mm = 1000 um;
          mil = 25400 nm;
    end units length;

  type coordinate is record
                       x, y : length;
                     end record coordinate;

  attribute cell_position : coordinate;

end package cell_attributes;



entity CPU is
end entity CPU;


-- code from book

architecture cell_based of CPU is

  component fpu is
                  port ( -- . . . );
                    -- not in book
                    port_name : bit := '0' );
                -- end not in book
  end component;

  use work.cell_attributes.all;

  attribute cell_position of the_fpu : label is ( 540 um, 1200 um );

  -- . . .

begin

  the_fpu : component fpu
    port map ( -- . . . );
      -- not in book
      port_name => open );
  -- end not in book

  -- . . .

end architecture cell_based;

-- end code from book
