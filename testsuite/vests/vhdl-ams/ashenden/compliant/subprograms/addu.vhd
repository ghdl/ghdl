
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

entity addu is
end entity addu;


architecture test of addu is

  subtype word32 is bit_vector(31 downto 0);

  -- code in book

  procedure addu ( a, b : in word32;
                   result : out word32;  overflow : out boolean ) is
    variable sum : word32;
    variable carry : bit := '0';
  begin
    for index in sum'reverse_range loop
      sum(index) := a(index) xor b(index) xor carry;
      carry := ( a(index) and b(index) ) or ( carry and ( a(index) xor b(index) ) );
    end loop;
    result := sum;
    overflow := carry = '1';
  end procedure addu;

  -- end code in book

begin

  stimulus : process is

    -- code in book (in text)

    variable PC, next_PC : word32;
    variable overflow_flag : boolean;
    -- . . .

    -- end code in book

  begin
    PC := X"0000_0010";

    -- code in book (in text)

    addu ( PC, X"0000_0004", next_PC, overflow_flag);

    -- end code in book

    PC := X"FFFF_FFFC";
    addu ( PC, X"0000_0004", next_PC, overflow_flag);

    wait;
  end process stimulus;

end architecture test;
