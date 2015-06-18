------------------------------------------------------------------------------
-- "standard_textio_additions" package contains the additions to the built in
-- "standard.textio" package.
-- This package should be compiled into "ieee_proposed" and used as follows:
-- use ieee_proposed.standard_textio_additions.all;
-- Last Modified: $Date: 2007/03/13 18:25:58 $
-- RCS ID: $Id: standard_textio_additions_c.vhdl,v 1.5 2007/03/13 18:25:58 l435385 Exp $
--
--  Created for VHDL-200X par, David Bishop (dbishop@vhdl.org)
------------------------------------------------------------------------------
use std.textio.all;
package standard_textio_additions is

--  procedure DEALLOCATE (P : inout LINE);

--  procedure FLUSH (file F : TEXT);
--
--  function MINIMUM (L, R : SIDE) return SIDE;
--  function MAXIMUM (L, R : SIDE) return SIDE;
--
--  function TO_STRING (VALUE : SIDE) return STRING;
--
--  function JUSTIFY (VALUE : STRING; JUSTIFIED : SIDE := right; FIELD : WIDTH := 0) return STRING;
--
--  procedure SREAD (L    : inout LINE; VALUE : out STRING; STRLEN : out NATURAL);
--  alias STRING_READ is SREAD [LINE, STRING, NATURAL];
--  alias BREAD is READ [LINE, BIT_VECTOR, BOOLEAN];
--  alias BREAD is READ [LINE, BIT_VECTOR];
--  alias BINARY_READ is READ [LINE, BIT_VECTOR, BOOLEAN];
--  alias BINARY_READ is READ [LINE, BIT_VECTOR];
--  procedure OREAD (L    : inout LINE; VALUE : out BIT_VECTOR; GOOD : out BOOLEAN);
--  procedure OREAD (L    : inout LINE; VALUE : out BIT_VECTOR);
--  alias OCTAL_READ is OREAD [LINE, BIT_VECTOR, BOOLEAN];
--  alias OCTAL_READ is OREAD [LINE, BIT_VECTOR];
--  procedure HREAD (L    : inout LINE; VALUE : out BIT_VECTOR; GOOD : out BOOLEAN);
--  procedure HREAD (L    : inout LINE; VALUE : out BIT_VECTOR);
--  alias HEX_READ is HREAD [LINE, BIT_VECTOR, BOOLEAN];
--  alias HEX_READ is HREAD [LINE, BIT_VECTOR];
--  procedure TEE (file F :     TEXT; L : inout LINE);
--  procedure WRITE (L      : inout LINE; VALUE : in REAL;
--                   FORMAT : in    STRING);
  alias SWRITE is WRITE [LINE, STRING, SIDE, WIDTH];
  alias STRING_WRITE is WRITE [LINE, STRING, SIDE, WIDTH];
  alias BWRITE is WRITE [LINE, BIT_VECTOR, SIDE, WIDTH];
  alias BINARY_WRITE is WRITE [LINE, BIT_VECTOR, SIDE, WIDTH];
  procedure OWRITE (L         : inout LINE; VALUE : in BIT_VECTOR;
                    JUSTIFIED : in    SIDE := right; FIELD : in WIDTH := 0);
  alias OCTAL_WRITE is OWRITE [LINE, BIT_VECTOR, SIDE, WIDTH];
  procedure HWRITE (L         : inout LINE; VALUE : in BIT_VECTOR;
                    JUSTIFIED : in    SIDE := right; FIELD : in WIDTH := 0);
  alias HEX_WRITE is HWRITE [LINE, BIT_VECTOR, SIDE, WIDTH];

end package standard_textio_additions;

library ieee_proposed;
use ieee_proposed.standard_additions.all;

package body standard_textio_additions is
  --pragma synthesis_off
  constant NUS  : STRING(2 to 1) := (others => ' ');     -- NULL array
  constant NBSP : CHARACTER      := CHARACTER'val(160);  -- space character

  -- Writes L to a file without modifying the contents of the line
  procedure TEE (file F : TEXT; L : inout LINE) is
  begin
    write (OUTPUT, L.all & LF);
    writeline(F, L);
  end procedure TEE;

  procedure FLUSH (file F: TEXT) is  -- Implicit
  begin
    file_close (F);
  end procedure FLUSH;

  -- Read and Write procedure for strings
  procedure SREAD (L      : inout LINE;
                   VALUE  : out   STRING;
                   STRLEN : out   natural) is
    variable ok     : BOOLEAN;
    variable c      : CHARACTER;
    -- Result is padded with space characters
    variable result : STRING (1 to VALUE'length) := (others => ' ');
  begin
    VALUE := result;
    loop                                -- skip white space
      read(L, c, ok);
      exit when (ok = false) or ((c /= ' ') and (c /= NBSP) and (c /= HT));
    end loop;
    -- Bail out if there was a bad read
    if not ok then
      STRLEN := 0;
      return;
    end if;
    result (1) := c;
    STRLEN     := 1;
    for i in 2 to VALUE'length loop
      read(L, c, ok);
      if (ok = false) or ((c = ' ') or (c = NBSP) or (c = HT)) then
        exit;
      else
        result (i) := c;
      end if;
      STRLEN := i;
    end loop;
    VALUE := result;
  end procedure SREAD;

  -- Hex Read and Write procedures for bit_vector.
  -- Procedure only visible internally.
  procedure Char2QuadBits (C           :     CHARACTER;
                           RESULT      : out BIT_VECTOR(3 downto 0);
                           GOOD        : out BOOLEAN;
                           ISSUE_ERROR : in  BOOLEAN) is
  begin
    case c is
      when '0'       => result := x"0"; good := true;
      when '1'       => result := x"1"; good := true;
      when '2'       => result := x"2"; good := true;
      when '3'       => result := x"3"; good := true;
      when '4'       => result := x"4"; good := true;
      when '5'       => result := x"5"; good := true;
      when '6'       => result := x"6"; good := true;
      when '7'       => result := x"7"; good := true;
      when '8'       => result := x"8"; good := true;
      when '9'       => result := x"9"; good := true;
      when 'A' | 'a' => result := x"A"; good := true;
      when 'B' | 'b' => result := x"B"; good := true;
      when 'C' | 'c' => result := x"C"; good := true;
      when 'D' | 'd' => result := x"D"; good := true;
      when 'E' | 'e' => result := x"E"; good := true;
      when 'F' | 'f' => result := x"F"; good := true;
      when others    =>
        assert not ISSUE_ERROR report
          "TEXTIO.HREAD Error: Read a '" & c &
          "', expected a Hex character (0-F)." severity error;
        GOOD := false;
    end case;
  end procedure Char2QuadBits;

  procedure HREAD (L     : inout LINE;
                   VALUE : out   BIT_VECTOR;
                   GOOD  : out   BOOLEAN) is
    variable ok  : BOOLEAN;
    variable c   : CHARACTER;
    constant ne  : INTEGER                    := (VALUE'length+3)/4;
    constant pad : INTEGER                    := ne*4 - VALUE'length;
    variable sv  : BIT_VECTOR (0 to ne*4 - 1) := (others => '0');
    variable s   : STRING(1 to ne-1);
  begin
    VALUE := (VALUE'range => '0');
    loop                                -- skip white space
      read(l, c, ok);
      exit when (ok = false) or ((c /= ' ') and (c /= NBSP) and (c /= HT));
    end loop;
    -- Bail out if there was a bad read
    if not ok then
      GOOD := false;
      return;
    end if;
    Char2QuadBits(c, sv(0 to 3), ok, false);
    if not ok then
      GOOD := false;
      return;
    end if;
    read(L, s, ok);
    if not ok then
      GOOD := false;
      return;
    end if;
    for i in 1 to ne-1 loop
      Char2QuadBits(s(i), sv(4*i to 4*i+3), ok, false);
      if not ok then
        GOOD := false;
        return;
      end if;
    end loop;
    if or_reduce (sv (0 to pad-1)) = '1' then
      GOOD := false;                    -- vector was truncated.
    else
      GOOD  := true;
      VALUE := sv (pad to sv'high);
    end if;
  end procedure HREAD;

  procedure HREAD (L     : inout LINE;
                   VALUE : out   BIT_VECTOR) is
    variable ok  : BOOLEAN;
    variable c   : CHARACTER;
    constant ne  : INTEGER                   := (VALUE'length+3)/4;
    constant pad : INTEGER                   := ne*4 - VALUE'length;
    variable sv  : BIT_VECTOR(0 to ne*4 - 1) := (others => '0');
    variable s   : STRING(1 to ne-1);
  begin
    VALUE := (VALUE'range => '0');
    loop                                -- skip white space
      read(l, c, ok);
      exit when (ok = false) or ((c /= ' ') and (c /= NBSP) and (c /= HT));
    end loop;
    -- Bail out if there was a bad read
    if not ok then
      report "TEXTIO.HREAD Error: Failed skipping white space"
        severity error;
      return;
    end if;
    Char2QuadBits(c, sv(0 to 3), ok, true);
    if not ok then
      return;
    end if;
    read(L, s, ok);
    if not ok then
      report "TEXTIO.HREAD Error: Failed to read the STRING"
        severity error;
      return;
    end if;
    for i in 1 to ne-1 loop
      Char2QuadBits(s(i), sv(4*i to 4*i+3), ok, true);
      if not ok then
        return;
      end if;
    end loop;
    if or_reduce (sv (0 to pad-1)) = '1' then
      report "TEXTIO.HREAD Error: Vector truncated"
        severity error;
    else
      VALUE := sv (pad to sv'high);
    end if;
  end procedure HREAD;

  procedure HWRITE (L         : inout LINE;
                    VALUE     : in    BIT_VECTOR;
                    JUSTIFIED : in    SIDE  := right;
                    FIELD     : in    WIDTH := 0) is
  begin
    write (L         => L,
           VALUE     => to_hstring(VALUE),
           JUSTIFIED => JUSTIFIED,
           FIELD     => FIELD);
  end procedure HWRITE;

  -- Procedure only visible internally.
  procedure Char2TriBits (C           :     CHARACTER;
                          RESULT      : out BIT_VECTOR(2 downto 0);
                          GOOD        : out BOOLEAN;
                          ISSUE_ERROR : in  BOOLEAN) is
  begin
    case c is
      when '0'    => result := o"0"; good := true;
      when '1'    => result := o"1"; good := true;
      when '2'    => result := o"2"; good := true;
      when '3'    => result := o"3"; good := true;
      when '4'    => result := o"4"; good := true;
      when '5'    => result := o"5"; good := true;
      when '6'    => result := o"6"; good := true;
      when '7'    => result := o"7"; good := true;
      when others =>
        assert not ISSUE_ERROR
          report
          "TEXTIO.OREAD Error: Read a '" & c &
          "', expected an Octal character (0-7)."
          severity error;
        GOOD := false;
    end case;
  end procedure Char2TriBits;

  -- Read and Write procedures for Octal values
  procedure OREAD (L     : inout LINE;
                   VALUE : out   BIT_VECTOR;
                   GOOD  : out   BOOLEAN) is
    variable ok  : BOOLEAN;
    variable c   : CHARACTER;
    constant ne  : INTEGER                   := (VALUE'length+2)/3;
    constant pad : INTEGER                   := ne*3 - VALUE'length;
    variable sv  : BIT_VECTOR(0 to ne*3 - 1) := (others => '0');
    variable s   : STRING(1 to ne-1);
  begin
    VALUE := (VALUE'range => '0');
    loop                                -- skip white space
      read(l, c, ok);
      exit when (ok = false) or ((c /= ' ') and (c /= NBSP) and (c /= HT));
    end loop;
    -- Bail out if there was a bad read
    if not ok then
      GOOD := false;
      return;
    end if;
    Char2TriBits(c, sv(0 to 2), ok, false);
    if not ok then
      GOOD := false;
      return;
    end if;
    read(L, s, ok);
    if not ok then
      GOOD := false;
      return;
    end if;
    for i in 1 to ne-1 loop
      Char2TriBits(s(i), sv(3*i to 3*i+2), ok, false);
      if not ok then
        GOOD := false;
        return;
      end if;
    end loop;
    if or_reduce (sv (0 to pad-1)) = '1' then
      GOOD := false;                    -- vector was truncated.
    else
      GOOD  := true;
      VALUE := sv (pad to sv'high);
    end if;
  end procedure OREAD;

  procedure OREAD (L     : inout LINE;
                   VALUE : out   BIT_VECTOR) is
    variable c   : CHARACTER;
    variable ok  : BOOLEAN;
    constant ne  : INTEGER                   := (VALUE'length+2)/3;
    constant pad : INTEGER                   := ne*3 - VALUE'length;
    variable sv  : BIT_VECTOR(0 to ne*3 - 1) := (others => '0');
    variable s   : STRING(1 to ne-1);
  begin
    VALUE := (VALUE'range => '0');
    loop                                -- skip white space
      read(l, c, ok);
      exit when (ok = false) or ((c /= ' ') and (c /= NBSP) and (c /= HT));
    end loop;
    -- Bail out if there was a bad read
    if not ok then
      report "TEXTIO.OREAD Error: Failed skipping white space"
        severity error;
      return;
    end if;
    Char2TriBits(c, sv(0 to 2), ok, true);
    if not ok then
      return;
    end if;
    read(L, s, ok);
    if not ok then
      report "TEXTIO.OREAD Error: Failed to read the STRING"
        severity error;
      return;
    end if;
    for i in 1 to ne-1 loop
      Char2TriBits(s(i), sv(3*i to 3*i+2), ok, true);
      if not ok then
        return;
      end if;
    end loop;
    if or_reduce (sv (0 to pad-1)) = '1' then
      report "TEXTIO.OREAD Error: Vector truncated"
        severity error;
    else
      VALUE := sv (pad to sv'high);
    end if;
  end procedure OREAD;

  procedure OWRITE (L         : inout LINE;
                    VALUE     : in    BIT_VECTOR;
                    JUSTIFIED : in    SIDE  := right;
                    FIELD     : in    WIDTH := 0) is
  begin
    write (L         => L,
           VALUE     => to_ostring(VALUE),
           JUSTIFIED => JUSTIFIED,
           FIELD     => FIELD);
  end procedure OWRITE;

  -- read and write for vector versions
  -- These versions produce "value1, value2, value3 ...."
  procedure read (L     : inout LINE;
                  VALUE : out   boolean_vector;
                  GOOD  : out   BOOLEAN) is
    variable dummy : CHARACTER;
    variable igood : BOOLEAN := true;
  begin
    for i in VALUE'range loop
      read (L     => L,
            VALUE => VALUE(i),
            GOOD  => igood);
      if (igood) and (i /= value'right) then
        read (L     => L,
              VALUE => dummy,           -- Toss the comma or seperator
              good  => igood);
      end if;
      if (not igood) then
        good := false;
        return;
      end if;
    end loop;
    good := true;
  end procedure read;

  procedure read (L     : inout LINE;
                  VALUE : out   boolean_vector) is
    variable dummy : CHARACTER;
    variable igood : BOOLEAN;
  begin
    for i in VALUE'range loop
      read (L     => L,
            VALUE => VALUE(i),
            good  => igood);
      if (igood) and (i /= value'right) then
        read (L     => L,
              VALUE => dummy,           -- Toss the comma or seperator
              good  => igood);
      end if;
      if (not igood) then
        report "STANDARD.STD_TEXTIO(BOOLEAN_VECTOR) "
          & "Read error ecounted during vector read" severity error;
        return;
      end if;
    end loop;
  end procedure read;

  procedure write (L         : inout LINE;
                   VALUE     : in    boolean_vector;
                   JUSTIFIED : in    SIDE  := right;
                   FIELD     : in    WIDTH := 0) is
  begin
    for i in VALUE'range loop
      write (L         => L,
             VALUE     => VALUE(i),
             JUSTIFIED => JUSTIFIED,
             FIELD     => FIELD);
      if (i /= value'right) then
        swrite (L, ", ");
      end if;
    end loop;
  end procedure write;

  procedure WRITE (L: inout LINE; VALUE: in REAL;
                   FORMAT: in STRING) is
  begin
--ET: Below modification is required to avoid the following error in iSim
--    "FATAL_ERROR:Simulator:CompilerAssert.h:40:1.67 - Internal Compiler Error in file ../src/VhdlExpr.cpp at line 7524   Process will terminate. For technical support on this issue, please open a WebCase with this project attached at http://www.xilinx.com/support."
--    swrite ( L => L, VALUE => to_string (VALUE, FORMAT));
	 swrite ( L, to_string (VALUE, FORMAT));
  end procedure WRITE;

  function justify (
    value     : STRING;
    justified : SIDE  := right;
    field     : width := 0)
    return STRING is
    constant VAL_LEN : INTEGER             := value'length;
    variable result  : STRING (1 to field) := (others => ' ');
  begin  -- function justify
    -- return value if field is too small
    if VAL_LEN >= field then
      return value;
    end if;
    if justified = left then
      result(1 to VAL_LEN) := value;
    elsif justified = right then
      result(field - VAL_LEN + 1 to field) := value;
    end if;
    return result;
  end function justify;

  function to_string (
    VALUE     : SIDE) return STRING is
  begin
    return SIDE'image(VALUE);
  end function to_string;

  -- pragma synthesis_on
  -- Will be implicit
  function minimum (L, R : SIDE) return SIDE is
  begin
    if L > R then return R;
    else return L;
    end if;
  end function minimum;

  function maximum (L, R : SIDE) return SIDE is
  begin
    if L > R then return L;
    else return R;
    end if;
  end function maximum;

end package body standard_textio_additions;
