
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
-- $Id: ch_05_ch_05_20.vhd,v 1.2 2001-10-26 16:29:34 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package pk_05_20 is

  -- code from book:

  type FIFO_status is record
                        nearly_full, nearly_empty, full, empty : bit;
                      end record FIFO_status;

  -- end of code from book

end package pk_05_20;


----------------------------------------------------------------


use work.pk_05_20.all;

entity FIFO is
  port ( status : out FIFO_status;
         other_ports : out bit );
end entity FIFO;


----------------------------------------------------------------


entity ch_05_20 is

end entity ch_05_20;


----------------------------------------------------------------


use work.pk_05_20.all;

architecture test of ch_05_20 is

  signal start_flush, end_flush, DMA_buffer_full, DMA_buffer_empty : bit;

begin

  -- code from book:

  DMA_buffer : entity work.FIFO
    port map ( -- . . ., 
      status.nearly_full => start_flush,
      status.nearly_empty => end_flush,
      status.full => DMA_buffer_full,
      status.empty => DMA_buffer_empty, -- . . . );
      -- not in book
      other_ports => open );
  -- end not in book

  -- end of code from book


end architecture test;
