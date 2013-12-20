
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

entity top is
end entity top;

--------------------------------------------------

architecture top_arch of top is

  signal top_sig : -- . . .;                  -- 1
    --
    bit;
    --

begin

  stimulus : process is
    variable var : -- . . .;                  -- 2
    --
    bit;
    --
  begin
    -- . . .
    --
    report "--1: " & top'path_name;
    report "--1: " & top'instance_name;
    report "--1: " & top_sig'path_name;
    report "--1: " & top_sig'instance_name;
    report "--2: " & stimulus'path_name;
    report "--2: " & stimulus'instance_name;
    report "--2: " & var'path_name;
    report "--2: " & var'instance_name;
    wait;
    --
  end process stimulus;

  rep_gen : for index in 0 to 7 generate
  begin

    end_gen : if index = 7 generate
      signal end_sig : -- . . .;              -- 3
      --
      bit;
      --
    begin
      -- . . .
      assert false report "--3: " & end_sig'path_name;
      assert false report "--3: " & end_sig'instance_name;
      --
    end generate end_gen;

    other_gen : if index /= 7 generate
      signal other_sig : -- . . .;            -- 4
      --
      bit;
      --
    begin
      other_comp : entity work.bottom(bottom_arch)
        port map ( -- . . . );
      --
          port_name => open );
      assert false report "--4: " & other_sig'path_name;
      assert false report "--4: " & other_sig'instance_name;
      --
    end generate other_gen;

  end generate rep_gen;

end architecture top_arch;
