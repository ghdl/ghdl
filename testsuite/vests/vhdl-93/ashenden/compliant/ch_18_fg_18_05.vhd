
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
-- $Id: ch_18_fg_18_05.vhd,v 1.2 2001-10-26 16:29:36 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity fg_18_05_a is
end entity fg_18_05_a;


architecture writer of fg_18_05_a is
begin

  process is

            type integer_file is file of integer;
          file data_file : integer_file open write_mode is "coeff-data";

  begin
    write(data_file, 0);
    write(data_file, 1);
    write(data_file, 2);
    write(data_file, 3);
    write(data_file, 4);
    write(data_file, 5);
    write(data_file, 6);
    write(data_file, 7);
    write(data_file, 8);
    write(data_file, 9);
    write(data_file, 10);
    write(data_file, 11);
    write(data_file, 12);
    write(data_file, 13);
    write(data_file, 14);
    write(data_file, 15);
    write(data_file, 16);
    write(data_file, 17);
    write(data_file, 18);

    wait;
  end process;

end architecture writer;



entity fg_18_05 is
end entity fg_18_05;


architecture test of fg_18_05 is
begin

  process is

            -- code from book (in text)

            type integer_vector is array (integer range <>) of integer;

          -- end code from book

          -- code from book (Figure 18-5)

          impure function read_array ( file_name : string;  array_length : natural )
            return integer_vector is
            type integer_file is file of integer;
            file data_file : integer_file open read_mode is file_name;
            variable result : integer_vector(1 to array_length) := (others => 0);
            variable index : integer := 1;
          begin
            while not endfile(data_file) and index <= array_length loop
              read(data_file, result(index));
              index := index + 1;
            end loop;
            return result;
          end function read_array;

          -- end code from book

          -- code from book (in text)

          constant coeffs : integer_vector := read_array("coeff-data", 16);

          -- end code from book

  begin
    wait;
  end process;

end architecture test;

