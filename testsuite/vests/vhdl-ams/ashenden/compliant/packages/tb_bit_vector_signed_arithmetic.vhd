
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

entity tb_bit_vector_signed_arithmetic is
end entity tb_bit_vector_signed_arithmetic;


architecture test of tb_bit_vector_signed_arithmetic is
begin

  stimulus : process is
    use work.bit_vector_signed_arithmetic.all;
    use std.textio.all;
    variable L : line;
  begin
    write(L, X"0002" + X"0005");
    writeline(output, L);
    write(L, X"0002" + X"FFFE");
    writeline(output, L);
    write(L, - X"0005");
    writeline(output, L);
    write(L, - X"FFFE");
    writeline(output, L);
    write(L, X"0002" * X"0005");
    writeline(output, L);
    write(L, X"0002" * X"FFFD");
    writeline(output, L);

    wait;
  end process stimulus;

end architecture test;
