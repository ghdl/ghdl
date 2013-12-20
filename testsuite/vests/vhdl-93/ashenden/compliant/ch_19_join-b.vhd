
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
-- $Id: ch_19_join-b.vhd,v 1.3 2001-10-26 16:29:36 paw Exp $
-- $Revision: 1.3 $
--
-- ---------------------------------------------------------------------

library qsim;

architecture behavior of join is

begin

  joiner : process

    use qsim.token_fifo_adt.all;

    variable source : positive range in_arc'range;
    variable token_fifo : fifo_type := new_fifo;
    variable current_fifo_size : natural := 0;
    variable head_token : token_type;
    variable number_of_tokens_joined : natural := 0;
    type counter_array is array (positive range in_arc'range) of natural;
    variable number_joined_from_source : counter_array := (others => 0);

    use std.textio.all;
    file info_file : text;
    variable L : line;

--  Modeltech bug mt043 workaround
    variable in_arc_last_value : arc_vector(in_arc'range) := in_arc;
--

    procedure write_summary is
    begin
      write(L, string'("Summary information for join "));
      write(L, name);
      write(L, string'(" up to time "));
      write(L, now, unit => time_unit);
      writeline(info_file, L);
      write(L, string'("  Number of tokens joined = "));
      write(L, natural(number_of_tokens_joined));
      writeline(info_file, L);
      for source in in_arc'range loop
        write(L, string'("    Number from input("));
        write(L, source);
        write(L, string'(") = "));
        write(L, natural(number_joined_from_source(source)));
        write(L, string'(" ("));
        write(L, real(number_joined_from_source(source))
              / real(number_of_tokens_joined),
              digits => 4);
        write(L, ')');
        writeline(info_file, L);
      end loop;
      writeline(info_file, L);
    end write_summary;

    procedure write_trace is
    begin
      write(L, string'("Join "));
      write(L, name);
      write(L, string'(": at "));
      write(L, now, unit => time_unit);
      write(L, string'(" joined from input "));
      write(L, source);
      write(L, ' ');
      write(L, in_arc(source).token, time_unit);
      writeline(info_file, L);
    end write_trace;

    procedure accept_new_tokens is
    begin
      for index in 1 to in_arc'length loop
--  Modeltech bug mt043 workaround
--      if in_arc(index).transaction /= in_arc'last_value(index).transaction then
        if in_arc(index).transaction /= in_arc_last_value(index).transaction then
--
          source := index;
          insert(token_fifo, in_arc(source).token);
          current_fifo_size := current_fifo_size + 1;
          number_of_tokens_joined := number_of_tokens_joined + 1;
          number_joined_from_source(source) := number_joined_from_source(source) + 1;
          if info_detail = trace then
            write_trace;
          end if;
        end if;
      end loop;
--  Modeltech bug mt043 workaround
      in_arc_last_value := in_arc;
--
    end procedure accept_new_tokens;

  begin
    file_open(info_file, info_file_name, write_mode);
    loop
      wait on info_detail'transaction, in_arc;
      if info_detail'active and info_detail = summary then
        write_summary;
      end if;
      if in_arc'event then
	accept_new_tokens;
        while current_fifo_size > 0 loop
          remove(token_fifo, head_token);
          current_fifo_size := current_fifo_size - 1;
	  out_arc <= arc_type'( transaction => not out_arc.transaction'driving_value,
			        token => head_token );
          wait for 0 fs;  -- delta delay before next output token
          if info_detail'active and info_detail = summary then
            write_summary;
          end if;
          if in_arc'event then
	    accept_new_tokens;
	  end if;
        end loop;
      end if;
    end loop;
  end process joiner;

end behavior;
