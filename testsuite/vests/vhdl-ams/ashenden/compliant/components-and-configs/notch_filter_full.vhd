
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

library cmos_lib;  use cmos_lib.bulk_cmos_nfet;

configuration full of notch_filter is
  
  for opamp_based  -- architecture of notch_filter
  
    for all : simple_opamp
      use entity work.opamp(struct);

      for struct  -- architecture of opamp
  
        for m1, m2 : nfet
          use entity bulk_cmos_nfet(detailed);
        end for;

        for others : nfet
          use entity bulk_cmos_nfet(basic);
        end for;

        -- ...
  
      end for;  -- end of architecture struct

    end for;

    -- ...  -- bindings for other component instances
  
  end for;  -- end of architecture opamp_based

end configuration full;
