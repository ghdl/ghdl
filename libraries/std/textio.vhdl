--  Std.Textio package declaration.  This file is part of GHDL.
--  This file was written from the clause 14.3 of the VHDL LRM.
--  Copyright (C) 2002 - 2014 Tristan Gingold
--
--  GHDL is free software; you can redistribute it and/or modify it under
--  the terms of the GNU General Public License as published by the Free
--  Software Foundation; either version 2, or (at your option) any later
--  version.
--
--  GHDL is distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
--  for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with GCC; see the file COPYING3.  If not see
--  <http://www.gnu.org/licenses/>.

package Textio is

-- type definitions for text i/o

  -- a LINE is a pointer to a string value.
  type Line is access String;

  --  A file of variable-length ASCII records.
  --  Note: in order to work correctly, the TEXT file type must be declared in
  --  the Textio package of library Std.  Otherwise, a file of string has a
  --  non-ASCII format.
  type text is file of String;

  type side is (right, left);	-- For justifying ouput data within fields.
  subtype width is natural;	-- For specifying widths of output fields.

  -- standard text files

--START-V08
  function Justify (Value: String;
                    Justified : Side := Right;
                    Field: Width := 0 ) return String;
--END-V08

  file input:  text is in "STD_INPUT";  --V87
  file output: text is out "STD_OUTPUT";  --V87
  file input : text open read_mode is "STD_INPUT";  --!V87
  file output : text open write_mode is "STD_OUTPUT";  --!V87

-- input routines for standard types

  procedure readline (variable f: in text; l: inout line); --V87
  procedure readline (file f: text; l: inout line); --!V87

  --  For READ procedures:
  --  In this implementation, any L is accepted (ie, there is no constraints
  --  on direction, or left bound).  Therefore, even variable of type LINE
  --  not initialized by READLINE are accepted.  Strictly speaking, this is
  --  not required by LRM, nor prevented.  However, other implementations may
  --  fail at parsing such Strings.
  --
  --  Also, in case of error (GOOD is false), this implementation do not
  --  modify L (as specified by the LRM) nor VALUE.
  --
  --  For READ procedures without a GOOD argument, an assertion fails in case
  --  of error.
  --
  --  In case of overflow (ie, if the number is out of the bounds of the type),
  --  the procedure will fail with an execution error.
  --  FIXME: this should not occur for a bad String.

  procedure read (l: inout line; value: out bit; good: out boolean);
  procedure read (l: inout line; value: out bit);

  procedure read (l: inout line; value: out bit_vector; good: out boolean);
  procedure read (l: inout line; value: out bit_vector);

  procedure read (l: inout line; value: out boolean; good: out boolean);
  procedure read (l: inout line; value: out boolean);

  procedure read (l: inout line; value: out character; good: out boolean);
  procedure read (l: inout line; value: out character);

  procedure read (l: inout line; value: out integer; good: out boolean);
  procedure read (l: inout line; value: out integer);

  procedure read (l: inout line; value: out real; good: out boolean);
  procedure read (l: inout line; value: out real);

  procedure read (l: inout line; value: out String; good: out boolean);
  procedure read (l: inout line; value: out String);

  --  This implementation requires no space after the unit identifier,
  --  ie "7.5 nsv" is parsed as 7.5 ns.
  --  The unit identifier can be in lower case, upper case or mixed case.
  procedure read (l: inout line; value: out time; good: out boolean);
  procedure read (l: inout line; value: out time);

--START-V08
  procedure Sread (L : inout Line; Value : out String; Strlen : out Natural);

  alias STRING_READ is SREAD [LINE, STRING, NATURAL];
  alias BREAD is READ [LINE, BIT_VECTOR, BOOLEAN];
  alias BREAD is READ [LINE, BIT_VECTOR];
  alias BINARY_READ is READ [LINE, BIT_VECTOR, BOOLEAN];
  alias BINARY_READ is READ [LINE, BIT_VECTOR];

  procedure Oread (L : inout Line; Value : out Bit_Vector; Good : out Boolean);
  procedure Oread (L : inout Line; Value : out Bit_Vector);

  alias OCTAL_READ is OREAD [LINE, BIT_VECTOR, BOOLEAN];
  alias OCTAL_READ is OREAD [LINE, BIT_VECTOR];

  procedure Hread (L : inout Line; Value : out Bit_Vector; Good : out Boolean);
  procedure Hread (L : inout Line; Value : out Bit_Vector);

  alias HEX_READ is HREAD [LINE, BIT_VECTOR, BOOLEAN];
  alias HEX_READ is HREAD [LINE, BIT_VECTOR];
--END-V08

-- output routines for standard types

  procedure writeline (variable f: out text; l: inout line); --V87
  procedure writeline (file f: text; l: inout line); --!V87

--START-V08
  procedure Tee (file f : Text; L : inout LINE);
--END-V08

  --  This implementation accept any value for all the types.
  procedure write
    (l: inout line; value: in bit;
    justified: in side := right; field: in width := 0);
  procedure write
    (l: inout line; value: in bit_vector;
    justified: in side := right; field: in width := 0);
  procedure write
    (l: inout line; value: in boolean;
    justified: in side := right; field: in width := 0);
  procedure write
    (l: inout line; value: in character;
    justified: in side := right; field: in width := 0);
  procedure write
    (l: inout line; value: in integer;
    justified: in side := right; field: in width := 0);
  procedure write
    (L: inout line; value: in real;
    justified: in side := right; field: in width := 0;
    digits: in natural := 0);
  procedure write
    (l: inout line; value: in String;
    justified: in side := right; field: in width := 0);

  --  UNIT must be a unit name declared in std.standard.  Of course, no rules
  --  in the core VHDL language prevent you from using a value that is not a
  --  unit (eg: 10 ns or even 5 fs).
  --  An assertion error message is generated in this case, and question mark
  --  (?) is written at the place of the unit name.
  procedure write
    (l: inout line; value : in time;
    justified: in side := right; field: in width := 0; unit : in TIME := ns);

--START-V08
  alias Swrite is write [Line, String, Side, Width];
  alias String_Write is Write [Line, String, Side, Width];

  alias Bwrite is write [Line, Bit_Vector, Side, Width];
  alias Binary_Write is write [Line, Bit_Vector, Side, Width];

  procedure Owrite (L : inout line; value : in Bit_Vector;
                    Justified : in Side := Right; Field : in Width := 0);
  alias Octal_Write is Owrite [Line, Bit_Vector, Side, Width];

  procedure Hwrite (L : inout line; value : in Bit_Vector;
                    Justified : in Side := Right; Field : in Width := 0);
  alias Hex_Write is Hwrite [Line, Bit_Vector, Side, Width];
--END-V08
end textio;
