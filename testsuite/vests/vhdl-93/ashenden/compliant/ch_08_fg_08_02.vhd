
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
-- $Id: ch_08_fg_08_02.vhd,v 1.2 2001-10-26 16:29:34 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

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


-- not in book

entity fg_08_02 is
end entity fg_08_02;


architecture test of fg_08_02 is

  use work.cpu_types.all;

  signal addr : address := X"000000";
  signal status : status_value := idle;
  signal mem_sel, int_sel, io_sel : bit;

begin

  dut : entity work.address_decoder
    port map ( addr => addr, status => status,
               mem_sel => mem_sel, int_sel => int_sel, io_sel => io_sel );

  stimulus : process is
  begin
    wait for 10 ns;

    status <= fetch;      wait for 10 ns;
    status <= mem_read;   wait for 10 ns;
    status <= mem_write;  wait for 10 ns;
    status <= io_read;    wait for 10 ns;
    status <= io_write;   wait for 10 ns;
    status <= int_ack;    wait for 10 ns;
    status <= idle;       wait for 10 ns;

    addr <= X"EFFFFF";    wait for 10 ns;
    status <= fetch;      wait for 10 ns;
    status <= mem_read;   wait for 10 ns;
    status <= mem_write;  wait for 10 ns;
    status <= io_read;    wait for 10 ns;
    status <= io_write;   wait for 10 ns;
    status <= int_ack;    wait for 10 ns;
    status <= idle;       wait for 10 ns;

    addr <= X"F00000";    wait for 10 ns;
    status <= fetch;      wait for 10 ns;
    status <= mem_read;   wait for 10 ns;
    status <= mem_write;  wait for 10 ns;
    status <= io_read;    wait for 10 ns;
    status <= io_write;   wait for 10 ns;
    status <= int_ack;    wait for 10 ns;
    status <= idle;       wait for 10 ns;

    addr <= X"FFFFFF";    wait for 10 ns;
    status <= fetch;      wait for 10 ns;
    status <= mem_read;   wait for 10 ns;
    status <= mem_write;  wait for 10 ns;
    status <= io_read;    wait for 10 ns;
    status <= io_write;   wait for 10 ns;
    status <= int_ack;    wait for 10 ns;
    status <= idle;       wait for 10 ns;

    wait;
  end process stimulus;

end architecture test;

-- end not in book
