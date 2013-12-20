
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
-- $Id: ch_19_fork-b.vhd,v 1.2 2001-10-26 16:29:36 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

architecture behavior of fork is

begin

  forker : process

    variable cumulative_probabilities : probability_vector(1 to probabilities'length);
    variable destination : positive range out_arc'range;
    variable probabilities_index : positive range probabilities'range;
    variable number_of_tokens_forked : natural := 0;
    type counter_array is array (positive range out_arc'range) of natural;
    variable number_forked_to_destination : counter_array := (others => 0);

    variable random_info : random_info_record;
    variable random_number : real;

    type transaction_vector is array (positive range <>) of boolean;
    variable out_arc_transaction_driving_value : transaction_vector(out_arc'range)
      := (others => false);

    use std.textio.all;
    file info_file : text;
    variable L : line;

    procedure write_summary is
    begin
      write(L, string'("Summary information for fork "));
      write(L, name);
      write(L, string'(" up to time "));
      write(L, now, unit => time_unit);
      writeline(info_file, L);
      write(L, string'("  Number of tokens forked = "));
      write(L, natural(number_of_tokens_forked));
      writeline(info_file, L);
      for destination in out_arc'range loop
        write(L, string'("    Number to output("));
        write(L, destination);
        write(L, string'(") = "));
        write(L, number_forked_to_destination(destination));
        write(L, string'(" ("));
        write(L, real(number_forked_to_destination(destination))
              / real(number_of_tokens_forked),
              digits => 4);
        write(L, ')');
        writeline(info_file, L);
      end loop;
      writeline(info_file, L);
    end write_summary;

    procedure write_trace is
    begin
      write(L, string'("Fork "));
      write(L, name);
      write(L, string'(": at "));
      write(L, now, unit => time_unit);
      write(L, string'(" forked to output "));
      write(L, destination);
      write(L, ' ');
      write(L, in_arc.token, time_unit);
      writeline(info_file, L);
    end write_trace;

  begin
    assert probabilities'length = out_arc'length - 1
      report "incorrent number of probabilities - should be "
      & integer'image(out_arc'length - 1) severity failure;
    cumulative_probabilities := probabilities;
    for index in 2 to cumulative_probabilities'length loop
      cumulative_probabilities(index) := cumulative_probabilities(index - 1)
					 + cumulative_probabilities(index);
    end loop;
    init_uniform( random_info,
                  lower_bound => 0.0, upper_bound => 1.0, seed => seed );
    file_open(info_file, info_file_name, write_mode);

    loop
      wait on info_detail'transaction, in_arc;
      if info_detail'active and info_detail = summary then
        write_summary;
      end if;
      if in_arc'event then
        generate_random(random_info, random_number);
        destination := out_arc'left;
        for index in 1 to cumulative_probabilities'length loop
          exit when random_number < cumulative_probabilities(index);
	  if out_arc'ascending then
	    destination := destination + 1;
	  else
	    destination := destination - 1;
	  end if;
        end loop;
        out_arc(destination) <= arc_type'( transaction => not out_arc_transaction_driving_value(destination),
					   token => in_arc.token );
	out_arc_transaction_driving_value(destination) := not out_arc_transaction_driving_value(destination);
        number_of_tokens_forked := number_of_tokens_forked + 1;
        number_forked_to_destination(destination)
          := number_forked_to_destination(destination) + 1;
        if info_detail = trace then
          write_trace;
        end if;
      end if;
    end loop;
  end process forker;

end behavior;
