
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
-- $Id: ch_19_qsimt.vhd,v 1.2 2001-10-26 16:29:36 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

--use std.textio.line;
use std.textio.all;

package qsim_types is

  constant name_max_length : natural := 20;
  type token_id_type is range 0 to integer'high;

  type token_type is record
                       source_name : string(1 to name_max_length);
                       source_name_length : natural;
                       id : token_id_type;
                       creation_time : time;
                     end record;

  type token_vector is array (positive range <>) of token_type;

  type arc_type is record
                     transaction : boolean;  -- flips when an arc changes
                     token : token_type;
                   end record arc_type;

  type arc_vector is array (positive range <>) of arc_type;

  type info_detail_type is (none, summary, trace);

  procedure write ( L : inout line;  t : in token_type;
  creation_time_unit : in time := ns );

end package qsim_types;
