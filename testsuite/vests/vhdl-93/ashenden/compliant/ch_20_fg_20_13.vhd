
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
-- $Id: ch_20_fg_20_13.vhd,v 1.2 2001-10-26 16:29:36 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity fg_20_13 is
end entity fg_20_13;


architecture test of fg_20_13 is

  attribute trace : string;

  subtype byte is bit_vector(7 downto 0);
  type byte_vector is array (natural range <>) of byte;

  type ram_bus is record
                    d : byte;
                    cmd, status, clk : bit;
                  end record ram_bus;

  -- code from book

  procedure mem_read ( address : in natural;
                       result : out byte_vector;
                       signal memory_bus : inout ram_bus ) is

    attribute trace of address : constant is "integer/hex";
    attribute trace of result : variable is "byte/multiple/hex";
    attribute trace of memory_bus : signal is
      "custom/command=rambus.cmd";
    -- . . .

  begin
    -- . . .
    -- not in book
    report address'trace;
    report result'trace;
    report memory_bus'trace;
    -- end not in book
  end procedure mem_read;

  -- end code from book

  signal memory_bus : ram_bus;

begin

  process is
            variable address : natural;
          variable result : byte_vector(0 to 3);
  begin
    mem_read ( address, result, memory_bus );
    wait;
  end process;

end architecture test;
