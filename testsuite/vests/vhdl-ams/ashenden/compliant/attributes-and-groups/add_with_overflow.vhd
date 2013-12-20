
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

entity add_with_overflow is
end entity add_with_overflow;


architecture test of add_with_overflow is
begin

-- code from book

process is

  procedure add_with_overflow ( a, b : in integer;
                                sum : out integer;
                                overflow : out boolean ) is -- . . .

    -- not in book
    begin
    end;
    -- end not in book

  procedure add_with_overflow ( a, b : in bit_vector;
                                sum : out bit_vector;
                                overflow : out boolean ) is -- . . .

    -- not in book
    begin
    end;
    -- end not in book

  attribute built_in : string;

  attribute built_in of
    add_with_overflow [ integer, integer,
                        integer, boolean ] : procedure is "int_add_overflow";

  attribute built_in of
    add_with_overflow [ bit_vector, bit_vector,
                        bit_vector, boolean ] : procedure is "bit_vector_add_overflow";

begin
  -- . . .
  -- not in book
  wait;
  -- end not in book
end process;

-- end code from book

end architecture test;
