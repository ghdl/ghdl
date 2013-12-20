
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

architecture arch of ent is

  type t is . . .;

  signal s : t;

  procedure p1 ( . . . ) is
    variable v1 : t;
  begin
    v1 := s;
  end procedure p1;

begin  -- arch

  proc1 : process is

    variable v2 : t;

    procedure p2 ( . . . ) is
      variable v3 : t;
    begin
      p1 ( v2, v3, . . . );
    end procedure p2;

  begin  -- proc1
    p2 ( v2, . . . );
  end process proc1;

  proc2 : process is
    . . .
  begin  -- proc2
    p1 ( . . . );
  end process proc2;

end architecture arch;
