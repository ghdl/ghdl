
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

entity inline_16 is

end entity inline_16;


----------------------------------------------------------------


architecture test of inline_16 is

  -- code from book:

  type time_stamp is record
      seconds : integer range 0 to 59;
      minutes : integer range 0 to 59;
      hours : integer range 0 to 23;
    end record time_stamp;

  -- end of code from book

begin


  process_4_a : process is

    -- code from book:

    variable sample_time, current_time : time_stamp;

    --

    constant midday : time_stamp := (0, 0, 12);

    -- end of code from book

    constant clock : integer := 79;
    variable sample_hour : integer;

  begin

    current_time := (30, 15, 2);

    -- code from book:

    sample_time := current_time;

    sample_hour := sample_time.hours;

    current_time.seconds := clock mod 60;

    -- end of code from book

    wait;
  end process process_4_a;


  process_4_b : process is

    type opcodes is (add, sub, addu, subu, jmp, breq, brne, ld, st, nop);
    type reg_number is range 0 to 31;

    type instruction is record
        opcode : opcodes;
        source_reg1, source_reg2, dest_reg : reg_number;
        displacement : integer;
      end record instruction;

    -- code from book:

    constant midday : time_stamp := (hours => 12, minutes => 0, seconds => 0);

    --

    constant nop_instr : instruction :=
        ( opcode => addu,
          source_reg1 | source_reg2 | dest_reg => 0,
          displacement => 0 );

    variable latest_event : time_stamp := (others => 0); -- initially midnight

    -- end of code from book

  begin
    wait;
  end process process_4_b;

end architecture test;
