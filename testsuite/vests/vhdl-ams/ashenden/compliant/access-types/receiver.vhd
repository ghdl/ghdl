
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

entity receiver is
end entity receiver;



architecture test of receiver is
begin

  -- code from book

  receiver : process is

    use work.bounded_buffer_adt.all;

    variable receive_buffer : bounded_buffer := new_bounded_buffer(2048);
    variable buffer_overrun, buffer_underrun : boolean;
    -- . . .

    -- not in book
    variable received_byte, check_byte : byte;
    -- end not in book

  begin
    -- . . .

    test_full(receive_buffer, buffer_overrun);
    if not buffer_overrun then
      write(receive_buffer, received_byte);
    end if;
    -- . . .

    test_empty(receive_buffer, buffer_underrun);
    if not buffer_underrun then
      read(receive_buffer, check_byte);
    end if;
    -- . . .

  end process receiver;

  -- end code from book

end architecture test;

