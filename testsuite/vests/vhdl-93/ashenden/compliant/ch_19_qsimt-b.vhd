
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
-- $Id: ch_19_qsimt-b.vhd,v 1.2 2001-10-26 16:29:36 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package body qsim_types is

  use std.textio.all;

  procedure write ( L : inout line;  t : in token_type;
  creation_time_unit : in time := ns ) is
  begin
    write(L, string'("token "));
    write(L, natural(t.id));
    write(L, string'(" from "));
    write(L, t.source_name(1 to t.source_name_length));
    write(L, string'(" created at "));
    write(L, t.creation_time, unit => creation_time_unit);
  end write;

end package body qsim_types;
