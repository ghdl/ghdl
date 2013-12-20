
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
-- $Id: ch_13_fg_13_03.vhd,v 1.2 2001-10-26 16:29:35 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

library ieee;  use ieee.std_logic_1164.all;

               use work.serial_interface_defs.all;

               entity serial_interface is
                 port ( clock_phi1, clock_phi2 : in std_logic;
                        serial_select : in std_logic;
                        reg_address : in reg_address_vector;
                        data : inout data_vector;
                        interrupt_request : out std_logic;
                        rx_serial_data : in std_logic;
                        tx_serial_data : out std_logic );
               end entity serial_interface;


-- not in book

               architecture test of serial_interface is
               begin
               end architecture test;

-- end not in book
