
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
-- $Id: ch_17_fg_17_08.vhd,v 1.2 2001-10-26 16:29:36 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package bounded_buffer_adt is

  subtype byte is bit_vector(0 to 7);

  type bounded_buffer_object;  -- private

  type bounded_buffer is access bounded_buffer_object;

  function new_bounded_buffer ( size : in positive ) return bounded_buffer;
  -- creates a bounded buffer object with 'size' bytes of storage

  procedure test_empty ( variable the_bounded_buffer : in bounded_buffer;
                         is_empty : out boolean );
  -- tests whether the bounded buffer is empty (i.e., no data to read)

  procedure test_full ( variable the_bounded_buffer : in bounded_buffer;
                        is_full : out boolean );
  -- tests whether the bounded buffer is full (i.e., no data can be written)

  procedure write ( the_bounded_buffer : inout bounded_buffer;  data : in byte );
  -- if the bounded buffer is not full, writes the data
  -- if it is full, assertion violation with severity failure

  procedure read ( the_bounded_buffer : inout bounded_buffer;  data : out byte );
  -- if the bounded buffer is not empty, read the first byte of data
  -- if it is empty, assertion violation with severity failure

  --------------------------------------------------

  -- the following types are private to the ADT

  type store_array is array (natural range <>) of byte;

  type store_ptr is access store_array;

  type bounded_buffer_object is record
                                  byte_count : natural;
                                  head_index, tail_index : natural;
                                  store : store_ptr;
                                end record bounded_buffer_object;

end package bounded_buffer_adt;
