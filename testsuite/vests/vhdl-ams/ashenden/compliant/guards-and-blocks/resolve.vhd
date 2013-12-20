
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

package resolve is

  -- code from book (in text)

  subtype byte is bit_vector(0 to 7);
  type byte_array is array (integer range <>) of byte;
  function resolve ( bytes : byte_array ) return byte;
  subtype resolved_byte is resolve byte;

  -- end code from book

end package resolve;


package body resolve is

  -- code from book

  function resolve ( bytes : byte_array ) return byte is
    variable result : byte := b"0000_0000";
  begin
    for index in bytes'range loop
      result := result or bytes(index);
    end loop;
    return result;
  end function resolve;

  -- end code from book

end package body resolve;

