
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

entity inline_02a is

end entity inline_02a;


architecture test of inline_02a is
begin

  block_1 : block is

    -- code from book

    quantity input1, input2, output : real;
    quantity amplified_input1, amplified_input2 : real;

    constant gain1 : real := 2.0;
    constant gain2 : real := 4.0;

    -- end code from book

  begin

    -- code from book

    amplified_input1 == input1 * gain1;
    amplified_input2 == input2 * gain2;
    output == amplified_input1 * amplified_input2;

    -- end code from book

  end block block_1;


  block_2 : block is

    quantity input1, input2, output : real;

    constant gain1 : real := 2.0;
    constant gain2 : real := 4.0;

  begin

    -- code from book

    output == input1 * gain1 * input2 * gain2;

    -- end code from book

  end block block_2;

  
end architecture test;
