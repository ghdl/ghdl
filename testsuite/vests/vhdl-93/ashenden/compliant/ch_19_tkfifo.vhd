
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
-- $Id: ch_19_tkfifo.vhd,v 1.3 2001-10-26 16:29:36 paw Exp $
-- $Revision: 1.3 $
--
-- ---------------------------------------------------------------------

library qsim;

package token_fifo_adt is

  alias element_type is qsim.qsim_types.token_type;

  type fifo_record;

  type fifo_type is access fifo_record;

  function new_fifo return fifo_type;

  procedure test_empty ( variable fifo : in fifo_type;
                         variable is_empty : out boolean );

  procedure insert ( fifo : inout fifo_type;
                     element : in element_type );

  procedure remove ( fifo : inout fifo_type;
                     element : out element_type );

  -- private types

  type fifo_entry_record;

  type fifo_entry is access fifo_entry_record;

  type fifo_entry_record is record
                              next_entry : fifo_entry;
                              element : element_type;
                            end record;

  type fifo_record is record
                        head_entry, tail_entry : fifo_entry;
                      end record;

end package token_fifo_adt;
