
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

entity inline_01 is

end entity inline_01;


----------------------------------------------------------------


architecture test of inline_01 is
begin


  process_2_a : process is

    type t1 is (t1_1, t1_2);
    type t2 is (t2_1, t2_2);
    type t3 is (t3_1, t3_2);
    type t4 is (t4_1, t4_2);

    constant v4 : t4 := t4_1;

    constant val1 : t1 := t1_1;
    constant val2 : t2 := t2_1;
    variable var3 : t3 := t3_1;
    constant val4 : t4 := t4_1;

    -- code from book:

    procedure p ( f1 : in t1;  f2 : in t2;  f3 : out t3;  f4 : in t4 := v4 ) is
    begin
      -- . . .
    end procedure p;

    -- end of code from book

  begin

    -- code from book:

    p ( val1, val2, var3, val4 );
    p ( f1 => val1, f2 => val2, f4 => val4, f3 => var3 );
    p ( val1, val2, f4 => open, f3 => var3 );
    p ( val1, val2, var3 );

    -- end of code from book

    wait;
  end process process_2_a;


end architecture test;
