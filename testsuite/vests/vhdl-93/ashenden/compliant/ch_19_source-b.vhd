
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
-- $Id: ch_19_source-b.vhd,v 1.2 2001-10-26 16:29:36 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

library math;

architecture behavior of source is

begin

  token_generator : process is

                              variable source_name : string(1 to name_max_length) := (others => ' ');
                            variable source_name_length : natural;
                            variable next_token_id : token_id_type := 0;
                            variable next_arrival_time : time;
                            variable number_of_tokens_generated : natural := 0;
                            variable inter_arrival_time : natural;  -- in time_unit
                            variable sum_of_inter_arrival_times : real := 0.0;  -- in time_unit
                            variable sum_of_squares_of_inter_arrival_times : real := 0.0;  --in time_unit**2

                            variable random_info : random_info_record;
                            variable random_number : real;

                            use std.textio.all;
                            file info_file : text;
                            variable L : line;

                            use math.math_real.sqrt;

                            procedure write_summary is
                              variable measured_mean_inter_arrival_time : real
                                := sum_of_inter_arrival_times / real(number_of_tokens_generated);
                              variable measured_std_dev_of_inter_arrival_times : real
                                := sqrt ( ( sum_of_squares_of_inter_arrival_times
                                            - sum_of_inter_arrival_times**2 / real(number_of_tokens_generated) )
                                          / real( number_of_tokens_generated - 1 ) );
                            begin
                              write(L, string'("Summary information for source "));
                              write(L, name);
                              write(L, string'(" up to time "));
                              write(L, now, unit => time_unit);
                              writeline(info_file, L);
                              write(L, string'("  Inter arrival distribution: "));
                              write(L, distribution_type'image(distribution));
                              write(L, string'(" with mean inter arrival time of "));
                              write(L, mean_inter_arrival_time, unit => time_unit);
                              writeline(info_file, L);
                              write(L, string'("  Number of tokens generated = "));
                              write(L, natural(next_token_id));
                              writeline(info_file, L);
                              write(L, string'("  Mean inter arrival time = "));
                              write(L, measured_mean_inter_arrival_time * time_unit, unit => time_unit);
                              writeline(info_file, L);
                              write(L, string'("  Standard deviation of inter arrival times = "));
                              write(L, measured_std_dev_of_inter_arrival_times * time_unit, unit => time_unit);
                              writeline(info_file, L);
                              writeline(info_file, L);
                            end procedure write_summary;

                            procedure write_trace is
                            begin
                              write(L, string'("Source "));
                              write(L, name);
                              write(L, string'(": at "));
                              write(L, now, unit => time_unit);
                              write(L, string'(" generated token "));
                              write(L, natural(next_token_id));
                              writeline(info_file, L);
                            end procedure write_trace;

  begin
    if name'length > name_max_length then
      source_name := name(1 to name_max_length);
      source_name_length := name_max_length;
    else
      source_name(1 to name'length) := name;
      source_name_length := name'length;
    end if;
    file_open(info_file, info_file_name, write_mode);

    case distribution is
      when fixed =>
        init_fixed(random_info, real(mean_inter_arrival_time / time_unit));
      when uniform =>
        init_uniform( random_info,
                      lower_bound => 0.0,
                      upper_bound => 2.0 * real(mean_inter_arrival_time / time_unit),
                      seed => seed );
      when exponential =>
        init_exponential( random_info,
                          mean => real(mean_inter_arrival_time / time_unit),
                          seed => seed );
    end case;

    loop
      generate_random(random_info, random_number);
      inter_arrival_time :=  natural(random_number);
      next_arrival_time := inter_arrival_time * time_unit + now;
      loop
        wait on info_detail'transaction for next_arrival_time - now;
        if info_detail'active and info_detail = summary then
          write_summary;
        end if;
        exit when next_arrival_time = now;
      end loop;
      out_arc <= arc_type'( transaction => not out_arc.transaction'driving_value,
		            token => token_type'( source_name => source_name,
                                                  source_name_length => source_name_length,
                                                  id => next_token_id,
                                                  creation_time => now ) );
      number_of_tokens_generated := number_of_tokens_generated + 1;
      sum_of_inter_arrival_times := sum_of_inter_arrival_times
                                    + real(inter_arrival_time);
      sum_of_squares_of_inter_arrival_times := sum_of_squares_of_inter_arrival_times
                                               + real(inter_arrival_time) ** 2;

      if info_detail = trace then
        write_trace;
      end if;
      next_token_id := next_token_id + 1;
    end loop;
  end process token_generator;

end architecture behavior;
