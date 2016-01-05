--  Std.Textio package body.  This file is part of GHDL.
--  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
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

package body textio is
--START-V08
  --  LRM08 16.4
  --  The JUSTIFY operation formats a string value within a field that is at
  --  least at long as required to contain the value.  Parameter FIELD
  --  specifies the desired field width.  Since the actual field width will
  --  always be at least large enough to hold the string value, the default
  --  value 0 for the FIELD parameter has the effect of causing the string
  --  value to be contained in a field of exactly the right widteh (i.e., no
  --  additional leading or tailing spaces).  Parameter JUSTIFIED specified
  --  wether the string value is to be right- or left-justified within the
  --  field; the default is right-justified.  If the FIELD parameter describes
  --  a field width larger than the number of characters in the string value,
  --  space characters are used to fill the remaining characters in the field.
  --
  --  TG: Note that the bounds of the result are not specified!
  function Justify (Value: String;
                    Justified : Side := Right;
                    Field: Width := 0 ) return String
  is
    constant len : Width := Value'Length;
  begin
    if Field <= Len then
      return Value;
    else
      case Justified is
        when Right =>
          return (1 to Field - Len => ' ') & Value;
        when Left =>
          return Value & (1 to Field - Len => ' ');
      end case;
    end if;
  end Justify;
--END-V08

  -- output routines for standard types

  --  TIME_NAMES associates time units with textual names.
  --  Textual names are in lower cases, since according to LRM93 14.3:
  --   when written, the identifier is expressed in lowercase characters.
  --  The length of the names are 3 characters, the last one may be a space
  --  for 2 characters long names.
  type time_unit is
    record
      val : time;
      name : string (1 to 3);
    end record;
  type time_names_type is array (1 to 8) of time_unit;
  constant time_names : time_names_type :=
    ((fs, "fs "), (ps, "ps "), (ns, "ns "), (us, "us "),
     (ms, "ms "), (sec, "sec"), (min, "min"), (hr, "hr "));

  --  Non breaking space character.                     --!V87
  constant nbsp : character := character'val (160);	--!V87

  function is_whitespace (c : character) return Boolean is
  begin
    case c is
      when ' '
        | NBSP --!V87
	| HT =>
        return True;
      when others =>
        return False;
    end case;
  end is_Whitespace;

  procedure writeline (variable f: out text; l: inout line) is --V87
  procedure writeline (file f: text; l: inout line) is --!V87
  begin
    if l = null then
      -- LRM93 14.3
      -- If parameter L contains a null access value at the start of the call,
      -- the a null string is written to the file.
      null;
    else
      -- LRM93 14.3
      -- Procedure WRITELINE causes the current line designated by parameter L
      -- to be written to the file and returns with the value of parameter L
      -- designating a null string.
      write (f, l.all);
      deallocate (l);
      l := new string'("");
    end if;
    write (f, (1 => LF));
  end writeline;

--START-V08
  procedure Tee (file f : Text; L : inout LINE) is
  begin
    --  LRM08 16.4 Package TEXTIO
    --  The procedure TEE additionally causes the current line to be written
    --  to the file OUTPUT.
    if l = null then
      null;
    else
      write (f, l.all);
      write (Output, l.all);
      deallocate (l);
      l := new string'("");
    end if;
    write (f, (1 => LF));
    write (output, (1 => LF));
  end Tee;
--END-V08
  
  procedure write
    (l: inout line; value: in string;
    justified: in side := right; field: in width := 0)
  is
    variable length: natural;
    variable nl: line;
  begin
    -- l can be null.
    if l = null then
      length := 0;
    else
      length := l.all'length;
    end if;
    if value'length < field then
      nl := new string (1 to length + field);
      if length /= 0 then
        nl (1 to length) := l.all;
      end if;
      if justified = right then
        nl (length + 1 to length + field - value'length) := (others => ' ');
        nl (nl.all'high - value'length + 1 to nl.all'high) := value;
      else
        nl (length + 1 to length + value'length) := value;
        nl (length + value'length + 1 to nl.all'high) := (others => ' ');
      end if;
    else
      nl := new string (1 to length + value'length);
      if length /= 0 then
        nl (1 to length) := l.all;
      end if;
      nl (length + 1 to nl.all'high) := value;
    end if;
    deallocate (l);
    l := nl;
  end write;

  procedure write
    (l: inout line; value: in integer;
    justified: in side := right; field: in width := 0)
  is
    variable str: string (11 downto 1);
    variable val: integer := value;
    variable digit: natural;
    variable index: natural := 0;
  begin
    --  Note: the absolute value of VAL cannot be directly taken, since
    --  it may be greather that the maximum value of an INTEGER.
    loop
      --  LRM93 7.2.6
      --  (A rem B) has the sign of A and an absolute value less then
      --   the absoulte value of B.
      digit := abs (val rem 10);
      val := val / 10;
      index := index + 1;
      str (index) := character'val(48 + digit);
      exit when val = 0;
    end loop;
    if value < 0 then
      index := index + 1;
      str(index) := '-';
    end if;
    write (l, str (index downto 1), justified, field);
  end write;

  procedure write
    (l: inout line; value: in boolean;
    justified: in side := right; field: in width := 0)
  is
  begin
    if value then
      write (l, string'("TRUE"), justified, field);
    else
      write (l, string'("FALSE"), justified, field);
    end if;
  end write;

  procedure write
    (l: inout line; value: in character;
    justified: in side := right; field: in width := 0)
  is
    variable str: string (1 to 1);
  begin
    str (1) := value;
    write (l, str, justified, field);
  end write;

  function bit_to_char (value : in bit) return character is
  begin
    case value is
      when '0' =>
        return '0';
      when '1' =>
        return '1';
    end case;
  end bit_to_char;

  procedure write
    (l: inout line; value: in bit;
    justified: in side := right; field: in width := 0)
  is
    variable str : string (1 to 1);
  begin
    str (1) := bit_to_char (value);
    write (l, str, justified, field);
  end write;

  procedure write
    (l: inout line; value: in bit_vector;
    justified: in side := right; field: in width := 0)
  is
    constant length : natural := value'length;
    alias n_value : bit_vector (1 to value'length) is value;
    variable str : string (1 to length);
  begin
    for i in str'range loop
      str (i) := bit_to_char (n_value (i));
    end loop;
    write (l, str, justified, field);
  end write;

  procedure write
    (l: inout line; value : in time;
    justified: in side := right; field: in width := 0; unit : in TIME := ns)
  is
    --  Copy of VALUE on which we are working.
    variable val : time := value;

    --  Copy of UNIT on which we are working.
    variable un : time := unit;

    --  Digit extract from VAL/UN.
    variable d : integer; -- natural range 0 to 9;

    --  Index for unit name.
    variable n : integer;

    --  Result.
    variable str : string (1 to 28);

    --  Current character in RES.
    variable pos : natural := 1;

    --  Add a character to STR.
    procedure add_char (c : character) is
    begin
      str (pos) := c;
      pos := pos + 1;
    end add_char;
  begin
    --  Note:
    --  Care is taken to avoid overflow.  Time may be 64 bits while integer
    --   may be only 32 bits.

    --  Handle sign.
    --  Note: VAL cannot be negated since its range may be not symetric
    --  around 0.
    if val < 0 ns then
      add_char ('-');
    end if;

    --  Search for the first digit.
    --  Note: we must start from unit, since all units are not a power of 10.
    --  Note: UN can be multiplied only after we know it is possible.  This
    --   is a to avoid overflow.
    if un <= 0 fs then
      assert false report "UNIT argument is not positive" severity error;
      un := 1 ns;
    end if;
    while val / 10 >= un or val / 10 <= -un loop
      un := un * 10;
    end loop;

    --  Extract digits one per one.
    loop
      d := val / un;
      add_char (character'val (abs d + character'pos ('0')));
      val := val - d * un;
      exit when val = 0 ns and un <= unit;
      if un = unit then
	add_char ('.');
      end if;
      --  Stop as soon as precision will be lost.
      --  This can happen only for hr and min.
      --  FIXME: change the algorithm to display all the digits.
      exit when (un / 10) * 10 /= un;
      un := un / 10;
    end loop;

    add_char (' ');

    --  Search the time unit name in the time table.
    n := 0;
    for i in time_names'range loop
      if time_names (i).val = unit then
	n := i;
	exit;
      end if;
    end loop;
    assert n /= 0 report "UNIT argument is not a unit name" severity error;
    if n = 0 then
      add_char ('?');
    else
      add_char (time_names (n).name (1));
      add_char (time_names (n).name (2));
      if time_names (n).name (3) /= ' ' then
	add_char (time_names (n).name (3));
      end if;
    end if;

    --  Write the result.
    write (l, str (1 to pos - 1), justified, field);
  end write;

  -- Parameter DIGITS specifies how many digits to the right of the decimal
  -- point are to be output when writing a real number; the default value 0
  -- indicates that the number should be output in standard form, consisting
  -- of a normalized mantissa plus exponent (e.g., 1.079236E23). If DIGITS is
  -- nonzero, then the real number is output as an integer part followed by
  -- '.' followed by the fractional part, using the specified number of digits
  -- (e.g., 3.14159).
  -- Note: Nan, +Inf, -Inf are not to be considered, since these numbers are
  --  not in the bounds defined by any real range.
  procedure write (L: inout line; value: in real;
                   justified: in side := right; field: in width := 0;
		   digits: in natural := 0)
  is
    --  STR contains the result of the conversion.
    variable str : string (1 to 320);

    --  POS is the index of the next character to be put in STR.
    variable pos : positive := str'left;

    --  VAL contains the value to be converted.
    variable val : real;

    --  The exponent or mantissa computed is stored in MANTISSA.  This is
    --  a signed number.
    variable mantissa : integer;

    variable b : boolean;
    variable d : natural;

    --  Append character C in STR.
    procedure add_char (c : character) is
    begin
      str (pos) := c;
      pos := pos + 1;
    end add_char;

    --  Add digit V in STR.
    procedure add_digit (v : natural) is
    begin
      add_char (character'val (character'pos ('0') + v));
    end add_digit;

    --  Add leading digit and substract it.
    procedure extract_leading_digit is
      variable d : natural range 0 to 10;
    begin
      --  Note: We need truncation but type conversion does rounding.
      --  FIXME: should consider precision.
      d := natural (val);
      if real (d) > val then
	d := d - 1;
      end if;

      val := (val - real (d)) * 10.0;

      add_digit (d);
    end extract_leading_digit;
  begin
    --  Handle sign.
    --  There is no overflow here, since with IEEE implementations, sign is
    --  independant of the mantissa.
    --  LRM93 14.3
    --  The sign is never written if the value is non-negative.
    if value < 0.0 then
      add_char ('-');
      val := -value;
    else
      val := value;
    end if;

    --  Compute the mantissa.
    --  FIXME: should do a dichotomy.
    if val = 0.0 then
      mantissa := 0;
    elsif val < 1.0 then
      mantissa := -1;
      while val * (10.0 ** (-mantissa)) < 1.0 loop
	mantissa := mantissa - 1;
      end loop;
    else
      mantissa := 0;
      while val / (10.0 ** mantissa) >= 10.0 loop
	mantissa := mantissa + 1;
      end loop;
    end if;

    --  Normalize VAL: in [0; 10[
    if mantissa >= 0 then
      val := val / (10.0 ** mantissa);
    else
      val := val * 10.0 ** (-mantissa);
    end if;

    if digits = 0 then
      for i in 0 to 15 loop
	extract_leading_digit;

	if i = 0 then
	  add_char ('.');
	end if;
	exit when i > 0 and val < 10.0 ** (i + 1 - 15);
      end loop;

      --  LRM93 14.3
      --  if the exponent is present, the `e' is written as a lower case
      --  character.
      add_char ('e');

      if mantissa < 0 then
	add_char ('-');
	mantissa := -mantissa;
      end if;
      b := false;
      for i in 4 downto 0 loop
	d := (mantissa / 10000) mod 10;
	if d /= 0 or b or i = 0 then
	  add_digit (d);
	  b := true;
	end if;
	mantissa := (mantissa - d * 10000) * 10;
      end loop;
    else
      if mantissa < 0 then
	add_char ('0');
	mantissa := mantissa + 1;
      else
	loop
	  extract_leading_digit;
	  exit when mantissa = 0;
	  mantissa := mantissa - 1;
	end loop;
      end if;
      add_char ('.');
      for i in 1 to digits loop
	if mantissa = 0 then
	  extract_leading_digit;
	else
	  add_char ('0');
	  mantissa := mantissa + 1;
	end if;
      end loop;
    end if;
    write (l, str (1 to pos - 1), justified, field);
  end write;

--START-V08
  procedure Owrite (L : inout line; value : in Bit_Vector;
                    Justified : in Side := Right; Field : in Width := 0) is
  begin
    write (l, to_ostring (value), justified, field);
  end Owrite;

  procedure Hwrite (L : inout line; value : in Bit_Vector;
                    Justified : in Side := Right; Field : in Width := 0) is
  begin
    write (l, to_hstring (value), justified, field);
  end Hwrite;
--END-V08
  
  procedure untruncated_text_read                              --V87
    (variable f : text; str : out string; len : out natural);  --V87
  procedure untruncated_text_read                              --!V87
    (file f : text; str : out string; len : out natural);      --!V87

  attribute foreign : string;                                  --V87
  attribute foreign of untruncated_text_read : procedure is "GHDL intrinsic";

  procedure untruncated_text_read
    (variable f : text; str : out string; len : out natural) is    --V87
    (file f : text; str : out string; len : out natural) is    --!V87
  begin
    assert false report "must not be called" severity failure;
  end untruncated_text_read;

  procedure readline (variable f: in text; l: inout line) --V87
  procedure readline (file f: text; l: inout line) --!V87
  is
    variable len, nlen, posn : natural;
    variable nl, old_l : line;
    variable str : string (1 to 128);
    variable is_eol : boolean;
  begin
    --  LRM93 14.3
    --  If parameter L contains a non-null access value at the start of the
    --  call, the object designated by that value is deallocated before the
    --  new object is created.
    if l /= null then
      deallocate (l);
    end if;

    -- We read the input in 128-byte chunks.
    -- We keep reading until we reach a newline or there is no more input.
    -- The loop invariant is that old_l is allocated and contains the
    -- previous chunks read, and posn = old_l.all'length.
    posn := 0;
    loop
      untruncated_text_read (f, str, len);
      exit when len = 0;
      if str (len) = LF or str (len) = CR then
        --  LRM 14.3
        --  The representation of the line does not contain the representation
        --  of the end of the line.
        is_eol := true;
        len := len - 1;
        --  End of line is any of LF/CR/CR+LF/LF+CR.
        if len > 0 and (str (len) = LF or str (len) = CR) then
          len := len - 1;
        end if;
      elsif endfile (f) then
        is_eol := true;
      else
        is_eol := false;
      end if;
      l := new string (1 to posn + len);
      if old_l /= null then
        l (1 to posn) := old_l (1 to posn);
        deallocate (old_l);
      end if;
      l (posn + 1 to posn + len) := str (1 to len);
      exit when is_eol;
      posn := posn + len;
      old_l := l;
    end loop;
  end readline;

  --  Replaces L with L (LEFT to/downto L'RIGHT)
  procedure trim (l : inout line; left : natural)
  is
    variable nl : line;
  begin
    if l = null then
      return;
    end if;
    if l'left < l'right then
      --  Ascending.
      if left > l'right then
	nl := new string'("");
      else
	nl := new string (left to l'right);
--      nl := new string (1 to l'right + 1 - left);
	nl.all := l (left to l'right);
      end if;
    else
      --  Descending
      if left < l'right then
	nl := new string'("");
      else
	nl := new string (left downto l'right);
--      nl := new string (left - l'right + 1 downto 1);
	nl.all := l (left downto l'right);
      end if;
    end if;
    deallocate (l);
    l := nl;
  end trim;

  --  Replaces L with L (LEFT + 1 to L'RIGHT or LEFT - 1 downto L'RIGHT)
  procedure trim_next (l : inout line; left : natural)
  is
    variable nl : line;
  begin
    if l = null then
      return;
    end if;
    if l'left < l'right then
      --  Ascending.
      trim (l, left + 1);
    else
      --  Descending
      trim (l, left - 1);
    end if;
  end trim_next;

  function to_lower (c : character) return character is
  begin
    if c >= 'A' and c <= 'Z' then
      return character'val (character'pos (c) + 32);
    else
      return c;
    end if;
  end to_lower;

  procedure read (l: inout line; value: out character; good: out boolean)
  is
    variable nl : line;
  begin
    if l = null or l'length = 0 then
      good := false;
    else
      value := l (l'left);
      trim_next (l, l'left);
      good := true;
    end if;
  end read;

  procedure read (l: inout line; value: out character)
  is
    variable res : boolean;
  begin
    read (l, value, res);
    assert res = true
      report "character read failure"
      severity failure;
  end read;

  procedure read (l: inout line; value: out bit; good: out boolean)
  is
  begin
    good := false;
    for i in l'range loop
      case l(i) is
	when ' '
	  | NBSP --!V87
	  | HT =>
	  null;
	when '1' =>
	  value := '1';
	  good := true;
	  trim_next (l, i);
	  return;
	when '0' =>
	  value := '0';
	  good := true;
	  trim_next (l, i);
	  return;
	when others =>
	  return;
      end case;
    end loop;
    return;
  end read;

  procedure read (l: inout line; value: out bit)
  is
    variable res : boolean;
  begin
    read (l, value, res);
    assert res = true
      report "bit read failure"
      severity failure;
  end read;

  procedure read (l: inout line; value: out bit_vector; good: out boolean)
  is
    --  Number of bit to parse.
    variable len : natural;

    variable pos, last : natural;
    variable res : bit_vector (1 to value'length);

    --  State of the previous byte:
    --  LEADING: blank before the bit vector.
    --  FOUND: bit of the vector.
    type state_type is (leading, found);
    variable state : state_type;
  begin
    --  Initialization.
    len := value'length;
    if len = 0 then
      --  If VALUE is a nul array, return now.
      --  L stay unchanged.
      --  FIXME: should blanks be removed ?
      good := true;
      return;
    end if;
    good := false;
    state := leading;
    pos := res'left;
    for i in l'range loop
      case l(i) is
	when ' '
	  | NBSP --!V87
	  | HT =>
	  case state is
	    when leading =>
	      null;
	    when found =>
	      return;
	  end case;
	when '1' | '0' =>
	  case state is
	    when leading =>
	      state := found;
	    when found =>
	      null;
	  end case;
	  if l(i) = '0' then
	    res (pos) := '0';
	  else
	    res (pos) := '1';
	  end if;
	  pos := pos + 1;
	  len := len - 1;
	  last := i;
	  exit when len = 0;
	when others =>
	  return;
      end case;
    end loop;

    if len /= 0 then
      --  Not enough bits.
      return;
    end if;

    --  Note: if LEN = 0, then FIRST and LAST have been set.
    good := true;
    value := res;
    trim_next (l, last);
    return;
  end read;

  procedure read (l: inout line; value: out bit_vector)
  is
    variable res : boolean;
  begin
    read (l, value, res);
    assert res = true
      report "bit_vector read failure"
      severity failure;
  end read;

  procedure read (l: inout line; value: out boolean; good: out boolean)
  is
    --  State:
    --  BLANK: space are being scaned.
    --  L_TF : T(rue) or F(alse) has been scanned.
    --  L_RA : (t)R(ue) or (f)A(lse) has been scanned.
    --  L_UL : (tr)U(e) or (fa)L(se) has been scanned.
    --  L_ES : (tru)E or (fal)S(e) has been scanned.
    type state_type is (blank, l_tf, l_ra, l_ul, l_es);
    variable state : state_type;

    --  Set to TRUE if T has been scanned, to FALSE if F has been scanned.
    variable res : boolean;

    variable c : character;
  begin
    --  By default, it is a failure.
    good := false;
    state := blank;
    for i in l'range loop
      c := l (i);
      case state is
	when blank =>
	  if is_whitespace (c) then
	    null;
	  elsif c = 'f' or c = 'T' then
	    res := true;
	    state := l_tf;
	  elsif c = 'f' or c = 'F' then
	    res := false;
	    state := l_tf;
	  else
	    return;
	  end if;
	when l_tf =>
	  if res = true and (c = 'r' or c = 'R') then
	    state := l_ra;
	  elsif res = false and (c = 'a' or C = 'A') then
	    state := l_ra;
	  else
	    return;
	  end if;
	when l_ra =>
	  if res = true and (c = 'u' or C = 'U') then
	    state := l_ul;
	  elsif res = false and (c = 'l' or c = 'L') then
	    state := l_ul;
	  else
	    return;
	  end if;
	when l_ul =>
	  if res = true and (c = 'e' or c = 'E') then
	    trim_next (l, i);
	    good := true;
	    value := true;
	    return;
	  elsif res = false and (c = 's' or c = 'S') then
	    state := l_es;
	  else
	    return;
	  end if;
	when l_es =>
	  if res = false and (c = 'e' or c = 'E') then
	    trim_next (l, i);
	    good := true;
	    value := false;
	    return;
	  else
	    return;
	  end if;
      end case;
    end loop;
    return;
  end read;

  procedure read (l: inout line; value: out boolean)
  is
    variable res : boolean;
  begin
    read (l, value, res);
    assert res = true
      report "boolean read failure"
      severity failure;
  end read;

  function char_to_nat (c : character) return natural
  is
  begin
    return character'pos (c) - character'pos ('0');
  end char_to_nat;

  procedure read (l: inout line; value: out integer; good: out boolean)
  is
    variable val : integer;
    variable d : natural;

    type state_t is (leading, sign, digits);
    variable cur_state : state_t := leading;
  begin
    val := 1;
    for i in l'range loop
      case cur_state is
	when leading =>
	  case l(i) is
	    when ' '
	      | NBSP	--!V87
	      | ht =>
	      null;
	    when '+' =>
	      cur_state := sign;
	    when '-' =>
	      val := -1;
	      cur_state := sign;
	    when '0' to '9' =>
	      val := char_to_nat (l(i));
	      cur_state := digits;
	    when others =>
	      good := false;
	      return;
	  end case;
	when sign =>
	  case l(i) is
	    when '0' to '9' =>
	      val := val * char_to_nat (l(i));
	      cur_state := digits;
	    when others =>
	      good := false;
	      return;
	  end case;
	when digits =>
	  case l(i) is
	    when '0' to '9' =>
              d := char_to_nat (l(i));
	      val := val * 10;
              if val < 0 then
                val := val - d;
              else
                val := val + d;
              end if;
	    when others =>
	      trim (l, i);
	      good := true;
	      value := val;
	      return;
	  end case;
      end case;
    end loop;
    deallocate (l);
    l := new string'("");
    if cur_state /= leading then
      good := true;
      value := val;
    else
      good := false;
    end if;
  end read;

  procedure read (l: inout line; value: out integer)
  is
    variable res : boolean;
  begin
    read (l, value, res);
    assert res = true
      report "integer read failure"
      severity failure;
  end read;

  procedure read (l: inout line; value: out real; good: out boolean)
  is
    --  The result.
    variable val : real;
    --  True if the result is negative.
    variable val_neg : boolean;

    --  Number of digits after the dot.
    variable nbr_dec : natural;

    --  Value of the exponent.
    variable exp : integer;
    --  True if the exponent is negative.
    variable exp_neg : boolean;

    --  The parsing is done with a state machine.
    --  LEADING: leading blank suppression.
    --  SIGN: a sign has been found.
    --  DIGITS: integer parts
    --  DECIMALS: digits after the dot.
    --  EXPONENT_SIGN: sign after "E"
    --  EXPONENT_1: first digit of the exponent.
    --  EXPONENT: digits of the exponent.
    type state_t is (leading, sign, digits, decimals,
		     exponent_sign, exponent_1, exponent);
    variable cur_state : state_t := leading;

    --  Set VALUE to the result, and set GOOD to TRUE.
    procedure set_value is
    begin
      good := true;

      if exp_neg then
	val := val * 10.0 ** (-exp);
      else
	val := val * 10.0 ** exp;
      end if;
      if val_neg then
	value := -val;
      else
	value := val;
      end if;
    end set_value;

  begin
    --  Initialization.
    val_neg := false;
    nbr_dec := 1;
    exp := 0;
    exp_neg := false;

    --  By default, parsing has failed.
    good := false;

    --  Iterate over all characters of the string.
    --  Return immediatly in case of parse error.
    --  Trim L and call SET_VALUE and return in case of success.
    for i in l'range loop
      case cur_state is
	when leading =>
	  case l(i) is
	    when ' '
	      | NBSP	--!V87
	      | ht =>
	      null;
	    when '+' =>
	      cur_state := sign;
	    when '-' =>
	      val_neg := true;
	      cur_state := sign;
	    when '0' to '9' =>
	      val := real (char_to_nat (l(i)));
	      cur_state := digits;
	    when others =>
	      return;
	  end case;
	when sign =>
	  case l(i) is
	    when '0' to '9' =>
	      val := real (char_to_nat (l(i)));
	      cur_state := digits;
	    when others =>
	      return;
	  end case;
	when digits =>
	  case l(i) is
	    when '0' to '9' =>
	      val := val * 10.0 + real (char_to_nat (l(i)));
	    when '.' =>
	      cur_state := decimals;
	    when others =>
	      --  A "." (dot) is required in the string.
	      return;
	  end case;
	when decimals =>
	  case l(i) is
	    when '0' to '9' =>
	      val := val + real (char_to_nat (l(i))) / (10.0 ** nbr_dec);
	      nbr_dec := nbr_dec + 1;
	    when 'e' | 'E' =>
	      --  "nnn.E" is erroneous.
	      if nbr_dec = 1 then
		return;
	      end if;
	      cur_state := exponent_sign;
	    when others =>
	      --  "nnn.XX" is erroneous.
	      if nbr_dec = 1 then
		return;
	      end if;
	      trim (l, i);
	      set_value;
	      return;
	  end case;
	when exponent_sign =>
	  case l(i) is
	    when '+' =>
	      cur_state := exponent_1;
	    when '-' =>
	      exp_neg := true;
	      cur_state := exponent_1;
	    when '0' to '9' =>
	      exp := char_to_nat (l(i));
	      cur_state := exponent;
	    when others =>
	      --  Error.
	      return;
	  end case;
	when exponent_1 | exponent =>
	  case l(i) is
	    when '0' to '9' =>
	      exp := exp * 10 + char_to_nat (l(i));
	      cur_state := exponent;
	    when others =>
	      trim (l, i);
	      set_value;
	      return;
	  end case;
      end case;
    end loop;

    --  End of string.
    case cur_state is
      when leading | sign | digits =>
	--  Erroneous.
	return;
      when decimals =>
	--  "nnn.XX" is erroneous.
	if nbr_dec = 1 then
	  return;
	end if;
      when exponent_sign =>
	--  Erroneous ("NNN.NNNE")
	return;
      when exponent_1 =>
	--  "NNN.NNNE-"
	return;
      when exponent =>
	null;
    end case;

    deallocate (l);
    l := new string'("");
    set_value;
  end read;

  procedure read (l: inout line; value: out real)
  is
    variable res : boolean;
  begin
    read (l, value, res);
    assert res = true
      report "real read failure"
      severity failure;
  end read;

  procedure read (l: inout line; value: out time; good: out boolean)
  is
    --  The result.
    variable res : time;

    --  UNIT is computed from the unit name, the exponent and the number of
    --  digits before the dot.  UNIT is the weight of the current digit.
    variable unit : time;

    --  Number of digits before the dot.
    variable nbr_digits : integer;

    --  True if a unit name has been found.  Used temporaly to know the status
    --  at the end of the search loop.
    variable unit_found : boolean;

    --  True if the number is negative.
    variable is_neg : boolean;

    --  Value of the exponent.
    variable exp : integer;

    --  True if the exponent is negative.
    variable exp_neg : boolean;

    --  Unit name extracted from the string.
    variable unit_name : string (1 to 3);

    --  state is the kind of the previous character parsed.
    --  LEADING: leading blanks
    --  SIGN: + or - as the first character of the number.
    --  DIGITS: digit of the integer part of the number.
    --  DOT: dot (.) after the integer part and before the decimal part.
    --  DECIMALS: digit of the decimal part.
    --  EXPONENT_MARK: e or E.
    --  EXPONENT_SIGN: + or - just after the exponent mark (E).
    --  EXPONENT: digit of the exponent.
    --  UNIT_BLANK: blank after the exponent.
    --  UNIT_1, UNIT_2, UNIT_3: first, second, third character of the unit.
    type state_type is (leading, sign, digits, dot, decimals,
			exponent_mark, exponent_sign, exponent,
			unit_blank, unit_1, unit_2, unit_3);
    variable state : state_type;

    --  Used during the second scan of the string, TRUE is digits is being
    --  scaned.
    variable has_digits : boolean;

    --  Position at the end of the string.
    variable pos : integer;

    --  Used to compute POS.
    variable length : integer;
  begin
    --  Initialization.
    --  Fail by default; therefore, in case of error, a return statement is
    --  ok.
    good := false;

    nbr_digits := 0;
    is_neg := false;
    exp := 0;
    exp_neg := false;
    res := 0 fs;

    --  Look for exponent and unit name.
    --  Parse the string: this loop checks the correctness of the format, and
    --  must return (GOOD has been set to FALSE) in case of error.
    --  Set: NBR_DIGITS, IS_NEG, EXP, EXP_NEG.
    state := leading;
    for i in l'range loop
      case l (i) is
	when ' '
	  | NBSP  --!V87
	  | HT =>
	  case state is
	    when leading | unit_blank =>
	      null;
	    when sign | dot | exponent_mark | exponent_sign =>
	      return;
	    when digits | decimals | exponent =>
	      state := unit_blank;
	    when unit_1 | unit_2 =>
	      exit;
	    when unit_3 =>
	      --  Cannot happen, since an exit is performed at unit_3.
	      assert false report "internal error" severity failure;
	  end case;
	when '+' | '-' =>
	  case state is
	    when leading =>
	      if l(i) = '-' then
		is_neg := true;
	      end if;
	      state := sign;
	    when exponent_mark =>
	      if l(i) = '-' then
		exp_neg := true;
	      end if;
	      state := exponent_sign;
	    when others =>
	      return;
	  end case;
	when '0' to '9' =>
	  case state is
	    when exponent_mark | exponent_sign | exponent =>
	      exp := exp * 10 + char_to_nat (l (i));
	      state := exponent;
	    when leading | sign | digits =>
	      --  Leading "0" are not significant.
	      if nbr_digits > 0 or l (i) /= '0' then
		nbr_digits := nbr_digits + 1;
	      end if;
	      state := digits;
	    when decimals =>
	      null;
	    when dot =>
	      state := decimals;
	    when others =>
	      return;
	  end case;
	when 'a' to 'z' | 'A' to 'Z' =>
	  case state is
 	    when digits | decimals =>
	      --  "E" has exponent mark.
	      if l (i) = 'e' or l(i) = 'E' then
		state := exponent_mark;
	      else
		return;
	      end if;
	    when unit_blank =>
	      unit_name (1) := to_lower (l(i));
	      state := unit_1;
	    when unit_1 =>
	      unit_name (2) := to_lower (l(i));
	      state := unit_2;
	      pos := i;
	    when unit_2 =>
	      unit_name (3) := to_lower (l(i));
	      state := unit_3;
	      exit;
	    when others =>
	      return;
	  end case;
	when '.' =>
	  case state is
	    when digits =>
	      state := decimals;
	    when others =>
	      exit;
	  end case;
	when others =>
          exit;
      end case;
    end loop;

    --  A unit name (2 or 3 letters) must have been found.
    --  The string may end anywhere.
    if state /= unit_2 and state /= unit_3 then
      return;
    end if;

    --  Compute EXP with the sign.
    if exp_neg then
      exp := -exp;
    end if;

    --  Search the unit name in the list of time names.
    unit_found := false;
    for i in time_names'range loop
      --  The first two characters must match (case insensitive).
      --  The third character must match if:
      --  * the unit name is a three characters identifier (ie, not a blank).
      --  * there is a third character in STR.
      if time_names (i).name (1) = unit_name (1)
	and time_names (i).name (2) = unit_name (2)
	and (time_names (i).name (3) = ' '
	     or time_names (i).name (3) = unit_name (3))
      then
	unit := time_names (i).val;
	unit_found := true;
	--  POS is set to the position of the first invalid character.
	if time_names (i).name (3) = ' ' then
	  length := 1;
	else
	  length := 2;
	end if;
	if l'left < l'right then
	  pos := pos + length;
	else
	  pos := pos - length;
	end if;
	exit;
      end if;
    end loop;
    if not unit_found then
      return;
    end if;

    --  Compute UNIT, the weight of the first non-significant character.
    nbr_digits := nbr_digits + exp - 1;
    if nbr_digits < 0 then
      unit := unit / 10 ** (-nbr_digits);
    else
      unit := unit * 10 ** nbr_digits;
    end if;

    --  HAS_DIGITS will be set as soon as a digit is found.
    --  No error is expected here (this has been checked during the first
    --  pass).
    has_digits := false;
    for i in l'range loop
      case l (i) is
	when ' '
	  | NBSP  --!V87
	  | HT =>
	  if has_digits then
	    exit;
	  end if;
	when '+' | '-' =>
	  if not has_digits then
	    has_digits := true;
	  else
	    assert false report "internal error" severity failure;
	    return;
	  end if;
	when '0' to '9' =>
	  --  Leading "0" are not significant.
	  if l (i) /= '0' or res /= 0 fs then
	    res := res + char_to_nat (l (i)) * unit;
	    unit := unit / 10;
	  end if;
	  has_digits := true;
	when 'a' to 'z' | 'A' to 'Z' =>
	  if has_digits then
	    exit;
	  else
	    assert false report "internal error" severity failure;
	    return;
	  end if;
	when '.' =>
	  if not has_digits then
	    assert false report "internal error" severity failure;
	    return;
	  end if;
	when others =>
	  assert false report "internal error" severity failure;
	  return;
      end case;
    end loop;

    --  Set VALUE.
    if is_neg then
      value := -res;
    else
      value := res;
    end if;
    good := true;
    trim (l, pos);
    return;
  end read;

  procedure read (l: inout line; value: out time)
  is
    variable res : boolean;
  begin
    read (l, value, res);
    assert res = true
      report "time read failure"
      severity failure;
  end read;

  procedure read (l: inout line; value: out string; good: out boolean)
  is
    constant len : natural := value'length;
  begin
    if l'length < len then
      good := false;
      return;
    end if;
    good := true;
    if len = 0 then
      return;
    end if;
    if l'left < l'right then
      --  Ascending (expected common case).
      value := l (l'left to l'left + len - 1);
      trim (l, l'left + len);
    elsif l'left = l'right then
      --  String of 1 character.  We don't know the direction and therefore
      --  can't use the code below which does a slice.
      value := l.all;
      deallocate (l);
      l := new string'("");
    else
      --  Descending.
      value := l (l'left downto l'left - len + 1);
      trim (l, l'left - len);
    end if;
  end read;

  procedure read (l: inout line; value: out string)
  is
    variable res : boolean;
  begin
    read (l, value, res);
    assert res = true
      report "string read failure"
      severity failure;
  end read;

--START-V08
  procedure Sread (L : inout Line; Value : out String; Strlen : out Natural)
  is
    constant maxlen : natural := Value'Length;
    alias value1 : string (1 to maxlen) is Value;
    variable skipping : boolean := True;
    variable f, len, nl_left : natural;
    variable nl : line;
  begin
    --  Skip leading spaces.  F designates the index of the first non-space
    --  character, LEN the length of the extracted string.
    len := 0;
    for i in l'range loop
      if skipping then
        if not is_whitespace (l (i)) then
          skipping := false;
          f := i;
          len := 1;
        end if;
      else
        exit when is_whitespace (l (i));
        len := len + 1;
        exit when len = maxlen;
      end if;
    end loop;

    --  Copy string.
    if l'ascending then
      value1 (1 to len) := l (f to f + len - 1);
    else
      value1 (1 to len) := l (f downto f - len + 1);
    end if;
    strlen := len;

    if l'ascending then
      if len = 0 then
        f := l'right + 1;
      end if;
      nl_left := f + len;
      nl := new string (nl_left to l'right);
      nl.all := l (nl_left to l'right);
    else
      if len = 0 then
        f := l'right - 1;
      end if;
      nl_left := f - len;
      nl := new string (nl_left downto l'right);
      nl.all := l (nl_left downto l'right);
    end if;
    deallocate (l);
    l := nl;
  end sread;

  subtype bv4 is bit_vector (1 to 4);

  function char_to_bv4 (c : character) return bv4 is
  begin
    case c is
      when '0' => return "0000";
      when '1' => return "0001";
      when '2' => return "0010";
      when '3' => return "0011";
      when '4' => return "0100";
      when '5' => return "0101";
      when '6' => return "0110";
      when '7' => return "0111";
      when '8' => return "1000";
      when '9' => return "1001";
      when 'a' | 'A' => return "1010";
      when 'b' | 'B' => return "1011";
      when 'c' | 'C' => return "1100";
      when 'd' | 'D' => return "1101";
      when 'e' | 'E' => return "1110";
      when 'f' | 'F' => return "1111";
      when others =>
        assert false report "bad hexa digit" severity failure;
    end case;
  end char_to_bv4;

  procedure Oread (L : inout Line; Value : out Bit_Vector; Good : out Boolean)
  is
    --  Length of Value
    constant vlen : natural := value'length;

    --  Number of octal digits for Value
    constant olen : natural := (vlen + 2) / 3;

    variable res : bit_vector (1 to olen * 3);

    --  Number of bit to parse.
    variable len : natural;

    variable pos : natural;

    --  Last character from LEN to be removed
    variable last : integer;

    --  State of the previous byte:
    --  SKIP: blank before the bit vector.
    --  DIGIT: previous character was a digit
    --  UNDERSCORE: was '_'
    type state_type is (skip, digit, underscore);
    variable state : state_type;
  begin
    --  Initialization.
    if vlen = 0 then
      --  If VALUE is a nul array, return now.
      --  L stay unchanged.
      --  FIXME: should blanks be removed ?
      good := true;
      return;
    end if;
    good := false;
    state := skip;
    pos := res'left;
    if l'ascending then
      last := l'left - 1;
    else
      last := l'left + 1;
    end if;
    for i in l'range loop
      case l (i) is
	when ' '
	  | NBSP
	  | HT =>
	  exit when state /= skip;
        when '_' =>
          exit when state /= digit;
          state := underscore;
        when '0' to '7' =>
          res (pos to pos + 2) := char_to_bv4 (l (i)) (2 to 4);
          last := i;
          state := digit;
          pos := pos + 3;
          --  LRM08 16.4
          --  Character removal and compostion also stops when the expected
          --  number of digits have been removed.
          exit when pos = res'right + 1;
        when others =>
          exit;
      end case;
    end loop;

    --  LRM08 16.4
    --  The OREAD or HEAD procedure does not succeed if less than the expected
    --  number of digits are removed.
    if pos /= res'right + 1 then
      return;
    end if;

    --  LRM08 16.4
    --  The rightmost value'length bits of the binary number are used to form
    --  the result for the VALUE parameter, [with a '0' element corresponding
    --  to a 0 bit and a '1' element corresponding to a 1 bit].  The OREAD or
    --  HREAD procedure does not succeed if any unused bits are 1.
    for i in 1 to res'right - vlen loop
      if res (i) = '1' then
        return;
      end if;
    end loop;

    Value := res (res'right - vlen + 1 to res'right);
    good := true;
    trim_next (l, last);
  end Oread;

  procedure Oread (L : inout Line; Value : out Bit_Vector)
  is
    variable res : boolean;
  begin
    Oread (l, value, res);
    assert res = true
      report "octal bit_vector read failure"
      severity failure;
  end Oread;

  procedure Hread (L : inout Line; Value : out Bit_Vector; Good : out Boolean)
  is
    --  Length of Value
    constant vlen : natural := value'length;

    --  Number of hexa digits for Value
    constant hlen : natural := (vlen + 3) / 4;

    variable res : bit_vector (1 to hlen * 4);

    --  Number of bit to parse.
    variable len : natural;

    variable pos : natural;

    --  Last character from LEN to be removed
    variable last : integer;

    --  State of the previous byte:
    --  SKIP: blank before the bit vector.
    --  DIGIT: previous character was a digit
    --  UNDERSCORE: was '_'
    type state_type is (skip, digit, underscore);
    variable state : state_type;
  begin
    --  Initialization.
    if vlen = 0 then
      --  If VALUE is a nul array, return now.
      --  L stay unchanged.
      --  FIXME: should blanks be removed ?
      good := true;
      return;
    end if;
    good := false;
    state := skip;
    pos := res'left;
    if l'ascending then
      last := l'left - 1;
    else
      last := l'left + 1;
    end if;
    for i in l'range loop
      case l (i) is
	when ' '
	  | NBSP
	  | HT =>
	  exit when state /= skip;
        when '_' =>
          exit when state /= digit;
          state := underscore;
        when '0' to '9' | 'a' to 'f' | 'A' to 'F' =>
          res (pos to pos + 3) := char_to_bv4 (l (i));
          last := i;
          state := digit;
          pos := pos + 4;
          --  LRM08 16.4
          --  Character removal and compostion also stops when the expected
          --  number of digits have been removed.
          exit when pos = res'right + 1;
        when others =>
          exit;
      end case;
    end loop;

    --  LRM08 16.4
    --  The OREAD or HEAD procedure does not succeed if less than the expected
    --  number of digits are removed.
    if pos /= res'right + 1 then
      return;
    end if;

    --  LRM08 16.4
    --  The rightmost value'length bits of the binary number are used to form
    --  the result for the VALUE parameter, [with a '0' element corresponding
    --  to a 0 bit and a '1' element corresponding to a 1 bit].  The OREAD or
    --  HREAD procedure does not succeed if any unused bits are 1.
    for i in 1 to res'right - vlen loop
      if res (i) = '1' then
        return;
      end if;
    end loop;

    Value := res (res'right - vlen + 1 to res'right);
    good := true;
    trim_next (l, last);
  end Hread;

  procedure Hread (L : inout Line; Value : out Bit_Vector)
  is
    variable res : boolean;
  begin
    Hread (l, value, res);
    assert res = true
      report "hexa bit_vector read failure"
      severity failure;
  end Hread;
--END-V08
end textio;
