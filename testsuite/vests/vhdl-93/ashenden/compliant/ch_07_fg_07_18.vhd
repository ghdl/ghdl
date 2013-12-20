
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
-- $Id: ch_07_fg_07_18.vhd,v 1.2 2001-10-26 16:29:34 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity fg_07_18 is
end entity fg_07_18;


architecture test of fg_07_18 is

  constant target_host_id : natural := 10;
  constant my_host_id : natural := 5;
  type pkt_types is (control_pkt, other_pkt);
  type pkt_header is record
                       dest, src : natural;
                       pkt_type : pkt_types;
                       seq : natural;
                     end record;

begin

  -- code from book

  network_driver : process is

                             constant seq_modulo : natural := 2**5;
                           subtype seq_number is natural range 0 to seq_modulo-1;
                           variable next_seq_number : seq_number := 0;
                           -- . . .
                           -- not in book
                           variable new_header : pkt_header;
                           -- end not in book

                           impure function generate_seq_number return seq_number is
                             variable number : seq_number;
                           begin
                             number := next_seq_number;
                             next_seq_number := (next_seq_number + 1) mod seq_modulo;
                             return number;
                           end function generate_seq_number;

  begin  -- network_driver
    -- not in book
    wait for 10 ns;
    -- end not in book
    -- . . .
    new_header := pkt_header'( dest => target_host_id,
                               src => my_host_id,
                               pkt_type => control_pkt,
                               seq => generate_seq_number );
    -- . . .
  end process network_driver;

  -- end code from book

end architecture test;
