
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
-- $Id: ch_18_fg_18_08.vhd,v 1.2 2001-10-26 16:29:36 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package textio is

  type line is access string;

  type text is file of string;

  type side is (right, left);

  subtype width is natural;

  file input : text open read_mode is "std_input";
  file output : text open write_mode is "std_output";

  procedure readline(file f: text; l: out line);

  procedure read ( L : inout line;  value: out bit;  good : out boolean );
  procedure read ( L : inout line;  value: out bit );

  procedure read ( L : inout line;  value: out bit_vector;  good : out boolean );
  procedure read ( L : inout line;  value: out bit_vector );

  procedure read ( L : inout line;  value: out boolean;  good : out boolean );
  procedure read ( L : inout line;  value: out boolean );

  procedure read ( L : inout line;  value: out character;  good : out boolean );
  procedure read ( L : inout line;  value: out character );

  procedure read ( L : inout line;  value: out integer;  good : out boolean );
  procedure read ( L : inout line;  value: out integer );

  procedure read ( L : inout line;  value: out real;  good : out boolean );
  procedure read ( L : inout line;  value: out real );

  procedure read ( L : inout line;  value: out string;  good : out boolean );
  procedure read ( L : inout line;  value: out string );

  procedure read ( L : inout line;  value: out time;  good : out boolean );
  procedure read ( L : inout line;  value: out time );

  procedure writeline ( file f : text;  L : inout line );

  procedure write ( L : inout line;  value : in bit;
  justified: in side := right;  field: in width := 0 );

  procedure write ( L : inout line;  value : in bit_vector;
  justified: in side := right;  field: in width := 0 );

  procedure write ( L : inout line;  value : in boolean;
  justified: in side := right;  field: in width := 0 );

  procedure write ( L : inout line;  value : in character;
  justified: in side := right;  field: in width := 0 );

  procedure write ( L : inout line;  value : in integer;
  justified: in side := right;  field: in width := 0 );

  procedure write ( L : inout line;  value : in real;
  justified: in side := right;  field: in width := 0;
  digits: in natural := 0 );

  procedure write ( L : inout line;  value : in string;
  justified: in side := right;  field: in width := 0 );

  procedure write ( L : inout line;  value : in time;
  justified: in side := right;  field: in width := 0;
  unit: in time := ns );

end package textio;
