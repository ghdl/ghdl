library ieee;
--! Standard packages
use ieee.Std_logic_1164.all;
use ieee.Numeric_std.all;
--! Implicit use of std libary
--library std;
use std.textio.all;
--
-- unit name: csv_file_reader package
--
--! @brief Package contains functions to read CSV file
--!
--! @author Ricardo Jasinski
--!
--! @version 2.0
--!
--! @details
--! CSV Reader package taken from Web
--! Latest commit 4e2d20f  on 10 Sep 2013 
--! Public license - no guarrentee
--! https://github.com/ricardo-jasinski/vhdl-csv-file-reader
--!
--! Modifications by CL add to include function to read HEX numberes
-- Define operations to read formatted data from a comma-separated-values file
-- (CSV file). To use this package:
--    1. Create a csv_file_reader:      variable csv: csv_file_reader_type;
--    2. Open a csv file:               csv.initialize("c:\file.csv");
--    3. Read one line at a time:       csv.readline;
--    4. Start reading values:          my_integer := csv.read_integer;
--    5. To read more values in the same line, call any of the read_* functions
--    6. To move to the next line, call csv.readline() again
package csv_file_reader_pkg is
	-- Maximum string length for read operations
	constant LINE_LENGTH_MAX : integer := 256;
	-- file reader type
	type csv_file_reader_type is protected
		-- Open the CSV text file to be used for subsequent read operations
		procedure initialize(file_pathname : string);
		-- Release (close) the associated CSV file
		procedure dispose;
		-- Read one line from the csv file, and keep it in the cache
		procedure readline;
		-- Read a string from the csv file and convert it to an integer
		impure function read_integer return integer;
		-- Read a string from the csv file as an  hex number and convert it to an integer
		-- convert to a unsigned integer depending if MSB is set
		impure function read_hex return integer;
		-- Read a string from the csv file and convert it to real
		impure function read_real return real;
		-- Read a string from the csv file and convert it to boolean
		impure function read_boolean return boolean;
		-- Read a string with a numeric value from the csv file and convert it to a boolean
		impure function read_integer_as_boolean return boolean;
		-- Read a string from the csv file, until a separator character ',' is found
		impure function read_string return string;
		-- True when the end of the CSV file was reached
		impure function end_of_file return boolean;
	end protected;
end;

package body csv_file_reader_pkg is

	type csv_file_reader_type is protected body
		file my_csv_file             : text;
		-- cache one line at a time for read operations
		variable current_line        : line;
		-- true when end of file was reached and there are no more lines to read
		variable end_of_file_reached : boolean;

		-- True when the end of the CSV file was reached
		impure function end_of_file return boolean is
		begin
			return end_of_file_reached;
		end;

		-- Open the CSV text file to be used for subsequent read operations
		procedure initialize(file_pathname : string) is
		begin
			file_open(my_csv_file, file_pathname, READ_MODE);
			end_of_file_reached := false;
		end;

		-- Release (close) the associated CSV file
		procedure dispose is
		begin
			file_close(my_csv_file);
		end;

		-- Read one line from the csv file, and keep it in the cache
		procedure readline is
		begin
			readline(my_csv_file, current_line);
			end_of_file_reached := endfile(my_csv_file);
		end;

		-- Skip a separator (comma character) in the current line
		procedure skip_separator is
			variable dummy_string : string(1 to LINE_LENGTH_MAX);
		begin
			dummy_string := read_string;
		end;

		-- Read a string from the csv file and convert it to integer
		impure function read_integer return integer is
			variable read_value : integer;
		begin
			-- read(current_line, read_value);
			skip_separator;
			return read_value;
		end;

		-- Read a string from the csv file and assume it is Hex value
		impure function read_hex return integer is
			variable return_integer : integer := 0;
			variable char_val       : integer := 0;
			variable read_char      : character;
			variable read_ok        : boolean := true;
		begin
			-- read(current_line, read_char, read_ok);
			while read_ok loop
				if read_char = ',' then
					return return_integer;
				else
					return_integer := return_integer * 16;
					if read_char >= 'a' and read_char <= 'f' then
						char_val := (character'pos(read_char) - character'pos('a')) + 10;
					elsif read_char >= 'A' and read_char <= 'F' then
						char_val := (character'pos(read_char) - character'pos('A')) + 10;
					else
						char_val := (character'pos(read_char) - character'pos('0'));
					end if;
					return_integer := return_integer + char_val;
				end if;
				-- read(current_line, read_char, read_ok);
			end loop;
			return read_integer;
		end;

		-- Read a string from the csv file and convert it to real
		impure function read_real return real is
			variable read_value : real;
		begin
			read(current_line, read_value);
			skip_separator;
			return read_value;
		end;

		-- Read a string from the csv file and convert it to boolean
		impure function read_boolean return boolean is
		begin
			return boolean'value(read_string);
		end;

		impure function read_integer_as_boolean return boolean is
		begin
			return (read_integer /= 0);
		end;

		-- Read a string from the csv file, until a separator character ',' is found
		-- Delimeter "" used if first and last char
		impure function read_string return string is
			variable return_string : string(1 to LINE_LENGTH_MAX);
			variable read_char     : character;
			variable read_ok       : boolean := true;
			variable read_delim    : boolean := false;
			variable index         : integer := 0;
		begin
			read(current_line, read_char, read_ok);
			while read_ok loop
				case index is
					when 0 =>
						if read_char = ',' then
							return return_string;
						elsif read_char = '"' then
							index      := 1;
							read_delim := true;
						else
							return_string(1) := read_char;
							index            := 2;
						end if;
					when -1 =>
						if read_char = ',' then
							return return_string;
						else
							read_delim := false;
						end if;
					when others =>
						if read_delim and read_char = '"' then
							index      := -1;
							read_delim := false;
						elsif not read_delim and read_char = ',' then
							return return_string;
						else
							return_string(index) := read_char;
							index                := index + 1;
						end if;
				end case;
				read(current_line, read_char, read_ok);
			end loop;
			return "Exit";
		end;
	end protected body;

end;

