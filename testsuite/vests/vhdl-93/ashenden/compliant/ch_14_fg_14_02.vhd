
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
-- $Id: ch_14_fg_14_02.vhd,v 1.2 2001-10-26 16:29:35 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

-- not in book

entity graphics_engine is
end entity graphics_engine;

-- end not in book


architecture behavioral of graphics_engine is

  type point is array (1 to 3) of real;
  type transformation_matrix is array (1 to 3, 1 to 3) of real;

  signal p, transformed_p : point;
  signal a : transformation_matrix;
  signal clock : bit;
  -- . . .

begin

  transform_stage : for i in 1 to 3 generate
  begin

    cross_product_transform : process is
                                        variable result1, result2, result3 : real := 0.0;
    begin
      wait until clock = '1';
      transformed_p(i) <= result3;
      result3 := result2;
      result2 := result1;
      result1 :=  a(i, 1) * p(1) + a(i, 2) * p(2) + a(i, 3) * p(3);
    end process cross_product_transform;

  end generate transform_stage;

  -- . . .    -- other stages in the pipeline, etc

  -- not in book

  clock_gen : clock <= '1' after 10 ns, '0' after 20 ns when clock = '0';

  stimulus : process is
  begin
    a <= ( (1.0, 0.0, 0.0), (0.0, 1.0, 0.0), (0.0, 0.0, 1.0) );
    p <= ( 10.0, 10.0, 10.0 );
    wait until clock = '0';
    p <= ( 20.0, 20.0, 20.0 );
    wait until clock = '0';
    p <= ( 30.0, 30.0, 30.0 );
    wait until clock = '0';
    p <= ( 40.0, 40.0, 40.0 );
    wait until clock = '0';
    p <= ( 50.0, 50.0, 50.0 );
    wait until clock = '0';
    p <= ( 60.0, 60.0, 60.0 );

    wait;
  end process stimulus;

  -- end not in book

end architecture behavioral;
