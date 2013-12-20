
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

entity stimulus_interpreter is
end entity stimulus_interpreter;


architecture test of stimulus_interpreter is

  quantity temperature : real;
  signal temp_sig, setting : real;
  signal enable, heater_fail : bit;

begin

-- code from book

stimulus_interpreter : process is

  use std.textio.all;

  file control : text open read_mode is "control";

  variable command : line;
  variable read_ok : boolean;
  variable next_time : time;
  variable whitespace : character;
  variable signal_id : string(1 to 4);
  variable temp_value, set_value : real;
  variable on_value, fail_value : bit;

begin

  command_loop : while not endfile(control) loop

    readline ( control, command );

    -- read next stimulus time, and suspend until then
    read ( command, next_time, read_ok );
    if not read_ok then
      report "error reading time from line: " & command.all
        severity warning;
      next command_loop;
    end if;
    wait for next_time - now;

    -- skip whitespace
    while command'length > 0
      and ( command(command'left) = ' '    -- ordinary space
            or command(command'left) = ' '  -- non-breaking space
            or command(command'left) = HT ) loop
      read ( command, whitespace );
    end loop;

    -- read signal identifier string
    read ( command, signal_id, read_ok );
    if not read_ok then
      report "error reading signal id from line: " & command.all
        severity warning;
      next command_loop;
    end if;
    -- dispatch based on signal id
    case signal_id is

      when "temp" =>
        read ( command, temp_value, read_ok );
        if not read_ok then
          report "error reading temperature value from line: "
                 & command.all
            severity warning;
          next command_loop;
        end if;
        temp_sig <= temp_value;

      when "set " =>
        -- . . .    -- similar to "temp"

        -- not in book
        read ( command, set_value, read_ok );
        if not read_ok then
          report "error reading setting value from line: "
                 & command.all
            severity warning;
          next command_loop;
        end if;
        setting <= set_value;
        -- end not in book

      when "on  " =>
        read ( command, on_value, read_ok );
        if not read_ok then
          report "error reading on value from line: "
                 & command.all
            severity warning;
          next command_loop;
        end if;
        enable <= on_value;

      when "fail" =>
        -- . . .    -- similar to "on  "

        -- not in book
        read ( command, fail_value, read_ok );
        if not read_ok then
          report "error reading fail value from line: "
                 & command.all
            severity warning;
          next command_loop;
        end if;
        heater_fail <= fail_value;
        -- end not in book

      when others =>
        report "invalid signal id in line: " & signal_id
          severity warning;
        next command_loop;

    end case;

  end loop command_loop;

  wait;

end process stimulus_interpreter;

-- end code from book

-- code from book (in text)

temperature == temp_sig'ramp;

-- end code from book (in text)

end architecture test;
