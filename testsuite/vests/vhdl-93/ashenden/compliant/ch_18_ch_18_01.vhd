
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
-- $Id: ch_18_ch_18_01.vhd,v 1.2 2001-10-26 16:29:36 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity ch_18_01 is

end entity ch_18_01;


----------------------------------------------------------------


architecture test of ch_18_01 is
begin


  process is

            -- code from book:

            type integer_file is file of integer;

          file lookup_table_file : integer_file is "lookup-values";

          -- end of code from book

  begin
    wait;
  end process;


  process is

            -- code from book:

            type file_open_kind is (read_mode, write_mode, append_mode);

          -- end of code from book

  begin
    wait;
  end process;


  process is

            type element_type is (t1, t2, t3);

          -- code from book:

          type file_type is file of element_type;

          procedure read ( file f : file_type;  value : out element_type );

          function endfile ( file f : file_type ) return boolean;

          -- end of code from book

          procedure read ( file f : file_type;  value : out element_type ) is
          begin
          end;

          function endfile ( file f : file_type ) return boolean is
          begin
          end;

  begin
    wait;
  end process;


end architecture test;
