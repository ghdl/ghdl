
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
-- $Id: ch_19_wtfifo-b.vhd,v 1.3 2001-10-26 16:29:36 paw Exp $
-- $Revision: 1.3 $
--
-- ---------------------------------------------------------------------

package body waiting_token_fifo_adt is

  function new_fifo return fifo_type is
  begin
    return new fifo_record'( null, null );
  end function new_fifo;


  procedure test_empty ( variable fifo : in fifo_type;
                         variable is_empty : out boolean ) is
  begin
    is_empty := fifo.head_entry = null;
  end procedure test_empty;


  procedure insert ( fifo : inout fifo_type;
                     element : in element_type ) is

    variable new_entry : fifo_entry
      := new fifo_entry_record'( next_entry => null,
                                 element => element );
  begin
    if fifo.tail_entry /= null then
      fifo.tail_entry.next_entry := new_entry;
    else
      fifo.head_entry := new_entry;
    end if;
    fifo.tail_entry := new_entry;
  end procedure insert;


  procedure remove ( fifo : inout fifo_type;
                     element : out element_type ) is
    variable empty_fifo : boolean;
    variable removed_entry : fifo_entry;
  begin
    test_empty(fifo, empty_fifo);
    if empty_fifo then
      report "remove from empty fifo" severity failure;
    else
      removed_entry := fifo.head_entry;
      element := removed_entry.element;
      fifo.head_entry := removed_entry.next_entry;
      if fifo.head_entry = null then  -- fifo now empty
        fifo.tail_entry := null;
      end if;
      deallocate(removed_entry);
    end if;
  end procedure remove;

end package body waiting_token_fifo_adt;
