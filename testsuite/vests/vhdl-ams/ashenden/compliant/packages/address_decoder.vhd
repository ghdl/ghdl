
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

entity address_decoder is
  port ( addr : in work.cpu_types.address;
         status : in work.cpu_types.status_value;
         mem_sel, int_sel, io_sel : out bit );
end entity address_decoder;

--------------------------------------------------

architecture functional of address_decoder is

  constant mem_low : work.cpu_types.address := X"000000";
  constant mem_high : work.cpu_types.address := X"EFFFFF";
  constant io_low : work.cpu_types.address := X"F00000";
  constant io_high : work.cpu_types.address := X"FFFFFF";

begin

  mem_decoder :
    mem_sel <= '1' when ( work.cpu_types."="(status, work.cpu_types.fetch)
                          or work.cpu_types."="(status, work.cpu_types.mem_read)
                          or work.cpu_types."="(status, work.cpu_types.mem_write) )
                        and addr >= mem_low
                        and addr <= mem_high else
               '0';

  int_decoder :
    int_sel <= '1' when work.cpu_types."="(status, work.cpu_types.int_ack) else
               '0';

  io_decoder :
    io_sel <= '1' when ( work.cpu_types."="(status, work.cpu_types.io_read)
                         or work.cpu_types."="(status, work.cpu_types.io_write) )
                       and addr >= io_low
                       and addr <= io_high else
              '0';

end architecture functional;
