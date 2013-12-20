
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

entity inline_02_write_data is
end entity inline_02_write_data;


architecture writer of inline_02_write_data is
begin

  process is
    type bit_vector_file is file of bit_vector;
    file vectors : bit_vector_file open write_mode is "vectors.dat";
  begin
    write(vectors, bit_vector'(""));
    write(vectors, bit_vector'("1"));
    write(vectors, bit_vector'("10"));
    write(vectors, bit_vector'("011"));
    write(vectors, bit_vector'("0100"));
    write(vectors, bit_vector'("00101"));
    write(vectors, bit_vector'("000110"));
    write(vectors, bit_vector'("0000111"));
    write(vectors, bit_vector'("00001000"));
    write(vectors, bit_vector'("111111111111111111111111111111111111111111111111111111111111111111111111"));
    wait;
  end process;

end architecture writer;


----------------------------------------------------------------



entity inline_02 is

end entity inline_02;


----------------------------------------------------------------


architecture test of inline_02 is
begin


  process is

   type element_type is (t1, t2, t3);
   type file_type is file of element_type;

    -- code from book:

    type bit_vector_file is file of bit_vector;

    procedure read ( file f : file_type;
                     value : out element_type;  length : out natural );

    -- end of code from book

    procedure read ( file f : file_type;
                     value : out element_type;  length : out natural ) is
    begin
    end;

  begin
    wait;
  end process;


  process is

    type bit_vector_file is file of bit_vector;

    -- code from book:

    file vectors : bit_vector_file open read_mode is "vectors.dat";
    variable next_vector : bit_vector(63 downto 0);
    variable actual_len : natural;

    -- end of code from book

    variable lost : boolean;

  begin
    while not endfile(vectors) loop

      -- code from book:

      read(vectors, next_vector, actual_len);

      -- end of code from book

      lost :=
      -- code from book:

      actual_len > next_vector'length

      -- end of code from book
      ;

    end loop;

    wait;
  end process;


end architecture test;
