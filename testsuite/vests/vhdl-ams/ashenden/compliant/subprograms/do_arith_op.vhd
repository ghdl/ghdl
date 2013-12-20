
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

entity do_arith_op is
end entity do_arith_op;


architecture test of do_arith_op is

  type func_code is (add, subtract);

  signal op1 : integer := 10;
  signal op2 : integer := 3;
  signal dest : integer := 0;
  signal func : func_code := add;

  signal Z_flag : boolean := false;

  constant Tpd : delay_length := 3 ns;

begin

  stimulus : process is

    -- code from book

    procedure do_arith_op ( op : in func_code ) is
      variable result : integer;
    begin
      case op is
        when add =>
          result := op1 + op2;
        when subtract =>
          result := op1 - op2;
      end case;
      dest  <=  result after Tpd;
      Z_flag  <=  result = 0 after Tpd;
    end procedure do_arith_op;

    -- end code from book

  begin
    wait for 10 ns;

    -- code from book (in text)

    do_arith_op ( add );

    -- end code from book

    wait for 10 ns;

    -- code from book (in text)

    do_arith_op ( func );

    -- end code from book

    wait for 10 ns;
    do_arith_op ( subtract );
    wait for 10 ns;
    op2 <= 10;
    wait for 10 ns;
    do_arith_op ( subtract );

    wait;
  end process stimulus;

end architecture test;
