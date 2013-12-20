
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
-- $Id: ch_04_fg_04_03.vhd,v 1.2 2001-10-26 16:29:33 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity fg_04_03 is

end entity fg_04_03;


----------------------------------------------------------------


architecture test of fg_04_03 is
begin

  -- code from book:

  modem_controller : process is

                               type symbol is ('a', 't', 'd', 'h', digit, cr, other);
                             type symbol_string is array (1 to 20) of symbol;
                             type state is range 0 to 6;
                             type transition_matrix is array (state, symbol) of state;

                             constant next_state : transition_matrix :=
                               ( 0 => ('a' => 1, others => 6),
                                 1 => ('t' => 2, others => 6),
                                 2 => ('d' => 3, 'h' => 5, others => 6),
                                 3 => (digit => 4, others => 6),
                                 4 => (digit => 4, cr => 0, others => 6),
                                 5 => (cr => 0, others => 6),
                                 6 => (cr => 0, others => 6) );

                             variable command : symbol_string;
                             variable current_state : state := 0;

                             -- not in book:
                             type sample_array is array (positive range <>) of symbol_string;
                             constant sample_command : sample_array :=
                               ( 1 => ( 'a', 't', 'd', digit, digit, cr, others => other ),
                                 2 => ( 'a', 't', 'h', cr, others => other ),
                                 3 => ( 'a', 't', other, other, cr, others => other ) );
                             -- end not in book

  begin
    -- . . .
    -- not in book:
    for command_index in sample_command'range loop
      command := sample_command(command_index);
      -- end not in book
      for index in 1 to 20 loop
        current_state := next_state( current_state, command(index) );
        case current_state is
          -- . . .
          -- not in book:
          when 0 => exit;
          when others => null;
                         -- end not in book
        end case;
      end loop;
      -- . . .
      -- not in book:
    end loop;
    wait;
    -- end not in book
  end process modem_controller;

  -- end of code from book

end architecture test;
