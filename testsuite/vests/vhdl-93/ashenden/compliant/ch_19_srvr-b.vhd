
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
-- $Id: ch_19_srvr-b.vhd,v 1.2 2001-10-26 16:29:36 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

library math;

architecture behavior of server is

begin

  service : process is

                      variable served_token : token_type;
                    variable release_time : time;
                    variable number_of_tokens_served : natural := 0;
                    variable service_time : natural;  -- in time_unit
                    variable sum_of_service_times : real := 0.0;  -- in time_unit
                    variable sum_of_squares_of_service_times : real := 0.0;  --in time_unit**2

                    variable random_info : random_info_record;
                    variable random_number : real;

                    use std.textio.all;
                    file info_file : text;
                    variable L : line;

                    use math.math_real.sqrt;

                    procedure write_summary is
                      variable measured_mean_service_time : real
                        := sum_of_service_times / real(number_of_tokens_served);
                      variable measured_std_dev_of_service_times : real
                        := sqrt ( ( sum_of_squares_of_service_times
                                    - sum_of_service_times**2 / real(number_of_tokens_served) )
                                  / real( number_of_tokens_served - 1 ) );
                    begin
                      write(L, string'("Summary information for server "));
                      write(L, name);
                      write(L, string'(" up to time "));
                      write(L, now, unit => time_unit);
                      writeline(info_file, L);
                      write(L, string'("  Service distribution: "));
                      write(L, distribution_type'image(distribution));
                      write(L, string'(" with mean service time of "));
                      write(L, mean_service_time, unit => time_unit);
                      writeline(info_file, L);
                      write(L, string'("  Number of tokens served = "));
                      write(L, natural(number_of_tokens_served));
                      writeline(info_file, L);
                      write(L, string'("  Mean service time = "));
                      write(L, measured_mean_service_time * time_unit, unit => time_unit);
                      writeline(info_file, L);
                      write(L, string'("  Standard deviation of service times = "));
                      write(L, measured_std_dev_of_service_times * time_unit, unit => time_unit);
                      writeline(info_file, L);
                      write(L, string'("  Utilization = "));
                      write(L, sum_of_service_times / real(now / time_unit), digits => 4);
                      writeline(info_file, L);
                      writeline(info_file, L);
                    end procedure write_summary;

                    procedure write_trace_service is
                    begin
                      write(L, string'("Server "));
                      write(L, name);
                      write(L, string'(": at "));
                      write(L, now, unit => time_unit);
                      write(L, string'(" served "));
                      write(L, in_arc.token, time_unit);
                      writeline(info_file, L);
                    end procedure write_trace_service;

                    procedure write_trace_release is
                    begin
                      write(L, string'("Server "));
                      write(L, name);
                      write(L, string'(": at "));
                      write(L, now, unit => time_unit);
                      write(L, string'(" released "));
                      write(L, served_token, time_unit);
                      writeline(info_file, L);
                    end procedure write_trace_release;

  begin
    file_open(info_file, info_file_name, write_mode);

    case distribution is
      when fixed =>
        init_fixed(random_info, real(mean_service_time / time_unit));
      when uniform =>
        init_uniform( random_info,
                      lower_bound => 0.0,
                      upper_bound => 2.0 * real(mean_service_time / time_unit),
                      seed => seed );
      when exponential =>
        init_exponential( random_info,
                          mean => real(mean_service_time / time_unit),
                          seed => seed );
    end case;

    in_ready <= true;
    loop
      wait on info_detail'transaction, in_arc;
      if info_detail'active and info_detail = summary then
        write_summary;
      end if;
      if in_arc'event then
	in_ready <= false;
        if info_detail = trace then
          write_trace_service;
        end if;
        served_token := in_arc.token;
        generate_random(random_info, random_number);
        service_time :=  natural(random_number);
        release_time := service_time * time_unit + now;
        loop
          wait on info_detail'transaction for release_time - now;
          if info_detail'active and info_detail = summary then
            write_summary;
          end if;
          exit when release_time = now;
        end loop;
	in_ready <= true;
	out_arc <= arc_type'( transaction => not out_arc.transaction'driving_value,
			      token => served_token );
        number_of_tokens_served := number_of_tokens_served + 1;
        sum_of_service_times := sum_of_service_times + real(service_time);
        sum_of_squares_of_service_times := sum_of_squares_of_service_times
                                           + real(service_time) ** 2;
        if info_detail = trace then
          write_trace_release;
        end if;
      end if;
    end loop;
  end process service;

end architecture behavior;
