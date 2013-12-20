
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
-- $Id: ch_19_queue-b.vhd,v 1.3 2001-10-26 16:29:36 paw Exp $
-- $Revision: 1.3 $
--
-- ---------------------------------------------------------------------

library math;

architecture behavior of queue is

begin

  queue_manager : process is

                            use qsim.queue_types.all, qsim.waiting_token_fifo_adt.all;

                          variable waiting_token, head_token : waiting_token_type;
                          variable waiting_token_fifo : fifo_type := new_fifo;
                          variable out_token_in_transit : boolean := false;
                          variable number_of_tokens_released : natural := 0;
                          variable current_queue_size : natural := 0;
                          variable maximum_queue_size : natural := 0;
                          variable waiting_time : natural;  -- in time_unit
                          variable sum_of_waiting_times : real := 0.0;  -- in time_unit
                          variable sum_of_squares_of_waiting_times : real := 0.0;  --in time_unit**2

                          use std.textio.all;
                          file info_file : text;
                          variable L : line;

                          use math.math_real.sqrt;

                          procedure write_summary is
                            variable mean_waiting_time : real
                              := sum_of_waiting_times / real(number_of_tokens_released);
                            variable std_dev_of_waiting_times : real
                              := sqrt ( ( sum_of_squares_of_waiting_times
                                          - sum_of_waiting_times**2 / real(number_of_tokens_released) )
                                        / real( number_of_tokens_released - 1 ) );
                          begin
                            write(L, string'("Summary information for queue "));
                            write(L, name);
                            write(L, string'(" up to time "));
                            write(L, now, unit => time_unit);
                            writeline(info_file, L);
                            write(L, string'("  Number of tokens currently waiting = "));
                            write(L, natural(current_queue_size));
                            writeline(info_file, L);
                            write(L, string'("  Number of tokens released = "));
                            write(L, natural(number_of_tokens_released));
                            writeline(info_file, L);
                            write(L, string'("  Maximum queue size = "));
                            write(L, natural(maximum_queue_size));
                            writeline(info_file, L);
                            write(L, string'("  Mean waiting time = "));
                            write(L, mean_waiting_time * time_unit, unit => time_unit);
                            writeline(info_file, L);
                            write(L, string'("  Standard deviation of waiting times = "));
                            write(L, std_dev_of_waiting_times * time_unit, unit => time_unit);
                            writeline(info_file, L);
                            writeline(info_file, L);
                          end procedure write_summary;

                          procedure write_trace_enqueue is
                          begin
                            write(L, string'("Queue "));
                            write(L, name);
                            write(L, string'(": at "));
                            write(L, now, unit => time_unit);
                            write(L, string'(" enqueued "));
                            write(L, waiting_token.token, time_unit);
                            writeline(info_file, L);
                          end procedure write_trace_enqueue;

                          procedure write_trace_dequeue is
                          begin
                            write(L, string'("Queue "));
                            write(L, name);
                            write(L, string'(": at "));
                            write(L, now, unit => time_unit);
                            write(L, string'(" dequeued "));
                            write(L, head_token.token, time_unit);
                            writeline(info_file, L);
                          end procedure write_trace_dequeue;

  begin
    file_open(info_file, info_file_name, write_mode);
    loop
      wait on info_detail'transaction, in_arc, out_ready;
      if info_detail'active and info_detail = summary then
        write_summary;
      end if;
      if in_arc'event then
        waiting_token := waiting_token_type'( token => in_arc.token,
                                              time_when_enqueued => now );
        insert(waiting_token_fifo, waiting_token);
        current_queue_size := current_queue_size + 1;
        if current_queue_size > maximum_queue_size then
          maximum_queue_size := current_queue_size;
        end if;
        if info_detail = trace then
          write_trace_enqueue;
        end if;
      end if;
      if out_ready and current_queue_size > 0 and not out_token_in_transit then
	remove(waiting_token_fifo, head_token);
	current_queue_size := current_queue_size - 1;
	out_arc <= arc_type'( transaction => not out_arc.transaction'driving_value,
			      token => head_token.token );
	out_token_in_transit := true;
	number_of_tokens_released := number_of_tokens_released + 1;
	waiting_time := (now - head_token.time_when_enqueued) / time_unit;
	sum_of_waiting_times := sum_of_waiting_times + real(waiting_time);
        sum_of_squares_of_waiting_times := sum_of_squares_of_waiting_times
                                           + real(waiting_time) ** 2;
        if info_detail = trace then
          write_trace_dequeue;
        end if;
      end if;
      if out_token_in_transit and not out_ready then
	out_token_in_transit := false;
      end if;
    end loop;
  end process queue_manager;

end architecture behavior;
