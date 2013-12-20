
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
-- $Id: ch_18_fg_18_01.vhd,v 1.2 2001-10-26 16:29:36 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

-- not in book

library ieee;  use ieee.std_logic_1164.all;

               entity fg_18_01_a is
               end entity fg_18_01_a;


               architecture writer of fg_18_01_a is
               begin

                 process is

                           subtype word is std_logic_vector(0 to 7);
                         type load_file_type is file of word;
                         file load_file : load_file_type open write_mode is "fg_18_01.dat";

                 begin
                   write(load_file, word'(X"00"));
                   write(load_file, word'(X"01"));
                   write(load_file, word'(X"02"));
                   write(load_file, word'(X"03"));
                   write(load_file, word'(X"04"));
                   write(load_file, word'(X"05"));
                   write(load_file, word'(X"06"));
                   write(load_file, word'(X"07"));
                   write(load_file, word'(X"08"));
                   write(load_file, word'(X"09"));
                   write(load_file, word'(X"0A"));
                   write(load_file, word'(X"0B"));
                   write(load_file, word'(X"0C"));
                   write(load_file, word'(X"0D"));
                   write(load_file, word'(X"0E"));
                   write(load_file, word'(X"0F"));

                   wait;
                 end process;

               end architecture writer;

-- end not in book


               library ieee;  use ieee.std_logic_1164.all;

               entity ROM is
                 generic ( load_file_name : string );
                 port ( sel : in std_logic;
                        address : in std_logic_vector;
                        data : inout std_logic_vector );
               end entity ROM;

--------------------------------------------------

               architecture behavioral of ROM is

               begin

                 behavior : process is

                                      subtype word is std_logic_vector(0 to data'length - 1);
                                    type storage_array is
                                      array (natural range 0 to 2**address'length - 1) of word;
                                    variable storage : storage_array;
                                    variable index : natural;
                                    -- . . .    -- other declarations

                                    type load_file_type is file of word;
                                    file load_file : load_file_type open read_mode is load_file_name;

                 begin

                   -- load ROM contents from load_file
                   index := 0;
                   while not endfile(load_file) loop
                     read(load_file, storage(index));
                     index := index + 1;
                   end loop;

                   -- respond to ROM accesses
                   loop
                     -- . . .
                   end loop;

                 end process behavior;

               end architecture behavioral;



-- not in book

               library ieee;  use ieee.std_logic_1164.all;

               entity fg_18_01 is
               end entity fg_18_01;


               architecture test of fg_18_01 is

                 signal sel : std_logic;
                 signal address : std_logic_vector(3 downto 0);
                 signal data : std_logic_vector(0 to 7);

               begin

                 dut : entity work.ROM(behavioral)
                   generic map ( load_file_name => "fg_18_01.dat" )
                   port map ( sel, address, data );

               end architecture test;

-- end not in book
