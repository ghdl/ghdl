
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
-- $Id: ch_13_fg_13_02.vhd,v 1.2 2001-10-26 16:29:35 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

library ieee;  use ieee.std_logic_1164.all;

               package serial_interface_defs is

                 subtype reg_address_vector is std_logic_vector(1 downto 0);

                 constant status_reg_address : reg_address_vector := B"00";
                 constant control_reg_address : reg_address_vector := B"01";
                 constant rx_data_register : reg_address_vector := B"10";
                 constant tx_data_register : reg_address_vector := B"11";

                 subtype data_vector is std_logic_vector(7 downto 0);

                 -- . . .    -- other useful declarations

                 component serial_interface is
                                              port ( clock_phi1, clock_phi2 : in std_logic;
                                                     serial_select : in std_logic;
                                                     reg_address : in reg_address_vector;
                                                     data : inout data_vector;
                                                     interrupt_request : out std_logic;
                                                     rx_serial_data : in std_logic;
                                                     tx_serial_data : out std_logic );
                 end component serial_interface;

               end package serial_interface_defs;
