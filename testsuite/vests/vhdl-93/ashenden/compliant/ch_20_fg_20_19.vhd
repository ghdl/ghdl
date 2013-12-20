
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
-- $Id: ch_20_fg_20_19.vhd,v 1.1.1.1 2001-08-22 18:20:48 paw Exp $
-- $Revision: 1.1.1.1 $
--
-- ---------------------------------------------------------------------

package display_interface is

  -- . . .

  -- not in book
  type status_type is (t1, t2, t3);
  -- end not in book

  procedure create_window ( size_x, size_y : natural;
                            status : out status_type );

  attribute foreign of create_window : procedure is
    "language Ada;  with window_operations;" &
    "bind to window_operations.create_window;" &
    "parameter size_x maps to size_x : in natural;" &
    "parameter size_y maps to size_y : in natural;" &
    "parameter status maps to status : out window_operations.status_type;" &
    "others map to default";

  -- . . .

end package display_interface;
