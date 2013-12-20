
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
-- $Id: ch_03_ch_03_12.vhd,v 1.3 2001-10-26 16:29:33 paw Exp $
-- $Revision: 1.3 $
--
-- ---------------------------------------------------------------------

entity ch_03_12 is
end entity ch_03_12;

architecture test of ch_03_12 is
begin

  process_3_4_a : process is

                            constant condition, condition_1,
                          condition_2, condition_3 : boolean := true;
                          variable index : integer;

  begin

    -- code from book: syntax check only

    -- change "condition" to roman italic

    -- not in book:
    loop
      -- end not in book

      if condition then
        exit;
      end if;

      -- not in book:
    end loop;
    -- end not in book

    --

    -- change "condition" to roman italic

    loop
      -- . . .
      exit when condition;
      -- . . .
    end loop;
    -- . . .      -- control transferred to here
    -- when condition becomes true within the loop

    --

    loop_name : loop
      -- . . .
      exit loop_name;
      -- . . .
    end loop loop_name ;

    --

    -- change conditions to roman italic with hyphens

    outer : loop
      -- . . .
      inner : loop
        -- . . .
        exit outer when condition_1; -- exit 1
        -- . . .
        exit when condition_2;       -- exit 2
        -- . . .
      end loop inner;
      -- . . .                      -- target A
      exit outer when condition_3;   -- exit 3
      -- . . .
    end loop outer;
    -- . . .                        -- target B

    --

    -- "statement..." in roman italic with hyphens

    loop
      -- statement_1;
      next when condition;
      -- statement_2;
    end loop;

    --

    -- "statement..." in roman italic with hyphens

    loop
      -- statement_1;
      if not condition then
        -- statement_2;
      end if;
    end loop;

    --

    while index > 0 loop
      -- . . .      -- statement A: do something with index
    end loop;
    -- . . .    -- statement B


    -- end of code from book

    wait;
  end process process_3_4_a;

end architecture test;
