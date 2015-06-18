------------------------------------------------------------------------------
-- "std_logic_1164_additions" package contains the additions to the standard
-- "std_logic_1164" package proposed by the VHDL-200X-ft working group.
-- This package should be compiled into "ieee_proposed" and used as follows:
-- use ieee.std_logic_1164.all;
-- use ieee_proposed.std_logic_1164_additions.all;
-- Last Modified: $Date: 2010/09/22 18:32:33 $
-- RCS ID: $Id: std_logic_1164_additions.vhdl,v 1.13 2010/09/22 18:32:33 l435385 Exp $
--
--  Created for VHDL-200X par, David Bishop (dbishop@vhdl.org)
------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use std.textio.all;
package std_logic_1164_additions is

  -- NOTE that in the new std_logic_1164, STD_LOGIC_VECTOR is a resolved
  -- subtype of STD_ULOGIC_VECTOR.  Thus there is no need for funcitons which
  -- take inputs in STD_LOGIC_VECTOR.
  -- For compatability with VHDL-2002, I have replicated all of these funcitons
  -- here for STD_LOGIC_VECTOR.
  -- new aliases
  alias to_bv is ieee.std_logic_1164.To_bitvector [STD_LOGIC_VECTOR, BIT return BIT_VECTOR];
  alias to_bv is ieee.std_logic_1164.To_bitvector [STD_ULOGIC_VECTOR, BIT return BIT_VECTOR];
  alias to_bit_vector is ieee.std_logic_1164.To_bitvector [STD_LOGIC_VECTOR, BIT return BIT_VECTOR];
  alias to_bit_vector is ieee.std_logic_1164.To_bitvector [STD_ULOGIC_VECTOR, BIT return BIT_VECTOR];
  alias to_slv is ieee.std_logic_1164.To_StdLogicVector [BIT_VECTOR return STD_LOGIC_VECTOR];
  alias to_slv is ieee.std_logic_1164.To_StdLogicVector [STD_ULOGIC_VECTOR return STD_LOGIC_VECTOR];
  alias to_std_logic_vector is ieee.std_logic_1164.To_StdLogicVector [BIT_VECTOR return STD_LOGIC_VECTOR];
  alias to_std_logic_vector is ieee.std_logic_1164.To_StdLogicVector [STD_ULOGIC_VECTOR return STD_LOGIC_VECTOR];
  alias to_sulv is ieee.std_logic_1164.To_StdULogicVector [BIT_VECTOR return STD_ULOGIC_VECTOR];
  alias to_sulv is ieee.std_logic_1164.To_StdULogicVector [STD_LOGIC_VECTOR return STD_ULOGIC_VECTOR];
  alias to_std_ulogic_vector is ieee.std_logic_1164.To_StdULogicVector [BIT_VECTOR return STD_ULOGIC_VECTOR];
  alias to_std_ulogic_vector is ieee.std_logic_1164.To_StdULogicVector [STD_LOGIC_VECTOR return STD_ULOGIC_VECTOR];

  function TO_01 (s : STD_ULOGIC_VECTOR; xmap : STD_ULOGIC := '0')
    return STD_ULOGIC_VECTOR;
  function TO_01 (s : STD_ULOGIC; xmap : STD_ULOGIC := '0')
    return STD_ULOGIC;
  function TO_01 (s : BIT_VECTOR; xmap : STD_ULOGIC := '0')
    return STD_ULOGIC_VECTOR;
  function TO_01 (s : BIT; xmap : STD_ULOGIC := '0')
    return STD_ULOGIC;

  -------------------------------------------------------------------
  -- overloaded shift operators
  -------------------------------------------------------------------

  function "sll" (l : STD_LOGIC_VECTOR; r : INTEGER) return STD_LOGIC_VECTOR;
  function "sll" (l : STD_ULOGIC_VECTOR; r : INTEGER) return STD_ULOGIC_VECTOR;

  function "srl" (l : STD_LOGIC_VECTOR; r : INTEGER) return STD_LOGIC_VECTOR;
  function "srl" (l : STD_ULOGIC_VECTOR; r : INTEGER) return STD_ULOGIC_VECTOR;

  function "rol" (l : STD_LOGIC_VECTOR; r : INTEGER) return STD_LOGIC_VECTOR;
  function "rol" (l : STD_ULOGIC_VECTOR; r : INTEGER) return STD_ULOGIC_VECTOR;

  function "ror" (l  : STD_LOGIC_VECTOR; r : INTEGER) return STD_LOGIC_VECTOR;
  function "ror" (l  : STD_ULOGIC_VECTOR; r : INTEGER) return STD_ULOGIC_VECTOR;
  -------------------------------------------------------------------
  -- vector/scalar overloaded logical operators
  -------------------------------------------------------------------
  function "and" (l  : STD_LOGIC_VECTOR; r : STD_ULOGIC) return STD_LOGIC_VECTOR;
  function "and" (l  : STD_ULOGIC_VECTOR; r : STD_ULOGIC) return STD_ULOGIC_VECTOR;
  function "and" (l  : STD_ULOGIC; r : STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR;
  function "and" (l  : STD_ULOGIC; r : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR;
  function "nand" (l : STD_LOGIC_VECTOR; r : STD_ULOGIC) return STD_LOGIC_VECTOR;
  function "nand" (l : STD_ULOGIC_VECTOR; r : STD_ULOGIC) return STD_ULOGIC_VECTOR;
  function "nand" (l : STD_ULOGIC; r : STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR;
  function "nand" (l : STD_ULOGIC; r : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR;
  function "or" (l   : STD_LOGIC_VECTOR; r : STD_ULOGIC) return STD_LOGIC_VECTOR;
  function "or" (l   : STD_ULOGIC_VECTOR; r : STD_ULOGIC) return STD_ULOGIC_VECTOR;
  function "or" (l   : STD_ULOGIC; r : STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR;
  function "or" (l   : STD_ULOGIC; r : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR;
  function "nor" (l  : STD_LOGIC_VECTOR; r : STD_ULOGIC) return STD_LOGIC_VECTOR;
  function "nor" (l  : STD_ULOGIC_VECTOR; r : STD_ULOGIC) return STD_ULOGIC_VECTOR;
  function "nor" (l  : STD_ULOGIC; r : STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR;
  function "nor" (l  : STD_ULOGIC; r : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR;
  function "xor" (l  : STD_LOGIC_VECTOR; r : STD_ULOGIC) return STD_LOGIC_VECTOR;
  function "xor" (l  : STD_ULOGIC_VECTOR; r : STD_ULOGIC) return STD_ULOGIC_VECTOR;
  function "xor" (l  : STD_ULOGIC; r : STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR;
  function "xor" (l  : STD_ULOGIC; r : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR;
  function "xnor" (l : STD_LOGIC_VECTOR; r : STD_ULOGIC) return STD_LOGIC_VECTOR;
  function "xnor" (l : STD_ULOGIC_VECTOR; r : STD_ULOGIC) return STD_ULOGIC_VECTOR;
  function "xnor" (l : STD_ULOGIC; r : STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR;
  function "xnor" (l : STD_ULOGIC; r : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR;

  -------------------------------------------------------------------
  -- vector-reduction functions.
  -- "and" functions default to "1", or defaults to "0"
  -------------------------------------------------------------------
  -----------------------------------------------------------------------------
  -- %%% Replace the "_reduce" functions with the ones commented out below.
  -----------------------------------------------------------------------------
  -- function "and" ( l  : std_logic_vector ) RETURN std_ulogic;
  -- function "and" ( l  : std_ulogic_vector ) RETURN std_ulogic;
  -- function "nand" ( l  : std_logic_vector ) RETURN std_ulogic;
  -- function "nand" ( l  : std_ulogic_vector ) RETURN std_ulogic;
  -- function "or" ( l  : std_logic_vector ) RETURN std_ulogic;
  -- function "or" ( l  : std_ulogic_vector ) RETURN std_ulogic;
  -- function "nor" ( l  : std_logic_vector ) RETURN std_ulogic;
  -- function "nor" ( l  : std_ulogic_vector ) RETURN std_ulogic;
  -- function "xor" ( l  : std_logic_vector ) RETURN std_ulogic;
  -- function "xor" ( l  : std_ulogic_vector ) RETURN std_ulogic;
  -- function "xnor" ( l  : std_logic_vector ) RETURN std_ulogic;
  -- function "xnor" ( l  : std_ulogic_vector ) RETURN std_ulogic;
  function and_reduce (l  : STD_LOGIC_VECTOR) return STD_ULOGIC;
  function and_reduce (l  : STD_ULOGIC_VECTOR) return STD_ULOGIC;
  function nand_reduce (l : STD_LOGIC_VECTOR) return STD_ULOGIC;
  function nand_reduce (l : STD_ULOGIC_VECTOR) return STD_ULOGIC;
  function or_reduce (l   : STD_LOGIC_VECTOR) return STD_ULOGIC;
  function or_reduce (l   : STD_ULOGIC_VECTOR) return STD_ULOGIC;
  function nor_reduce (l  : STD_LOGIC_VECTOR) return STD_ULOGIC;
  function nor_reduce (l  : STD_ULOGIC_VECTOR) return STD_ULOGIC;
  function xor_reduce (l  : STD_LOGIC_VECTOR) return STD_ULOGIC;
  function xor_reduce (l  : STD_ULOGIC_VECTOR) return STD_ULOGIC;
  function xnor_reduce (l : STD_LOGIC_VECTOR) return STD_ULOGIC;
  function xnor_reduce (l : STD_ULOGIC_VECTOR) return STD_ULOGIC;
  -------------------------------------------------------------------
  -- ?= operators, same functionality as 1076.3 1994 std_match
  -------------------------------------------------------------------
--  FUNCTION "?=" ( l, r : std_ulogic ) RETURN std_ulogic;
--  FUNCTION "?=" ( l, r : std_logic_vector ) RETURN std_ulogic;
--  FUNCTION "?=" ( l, r : std_ulogic_vector ) RETURN std_ulogic;
--  FUNCTION "?/=" ( l, r : std_ulogic ) RETURN std_ulogic;
--  FUNCTION "?/=" ( l, r : std_logic_vector ) RETURN std_ulogic;
--  FUNCTION "?/=" ( l, r : std_ulogic_vector ) RETURN std_ulogic;
--  FUNCTION "?>" ( l, r : std_ulogic ) RETURN std_ulogic;
--  FUNCTION "?>=" ( l, r : std_ulogic ) RETURN std_ulogic;
--  FUNCTION "?<" ( l, r : std_ulogic ) RETURN std_ulogic;
--  FUNCTION "?<=" ( l, r : std_ulogic ) RETURN std_ulogic;

  function \?=\ (l, r  : STD_ULOGIC) return STD_ULOGIC;
  function \?=\ (l, r  : STD_LOGIC_VECTOR) return STD_ULOGIC;
  function \?=\ (l, r  : STD_ULOGIC_VECTOR) return STD_ULOGIC;
  function \?/=\ (l, r : STD_ULOGIC) return STD_ULOGIC;
  function \?/=\ (l, r : STD_LOGIC_VECTOR) return STD_ULOGIC;
  function \?/=\ (l, r : STD_ULOGIC_VECTOR) return STD_ULOGIC;
  function \?>\ (l, r  : STD_ULOGIC) return STD_ULOGIC;
  function \?>=\ (l, r : STD_ULOGIC) return STD_ULOGIC;
  function \?<\ (l, r  : STD_ULOGIC) return STD_ULOGIC;
  function \?<=\ (l, r : STD_ULOGIC) return STD_ULOGIC;


  -- "??" operator, converts a std_ulogic to a boolean.
  --%%% Uncomment the following operators
  -- FUNCTION "??" (S : STD_ULOGIC) RETURN BOOLEAN;
  --%%% REMOVE the following funciton (for testing only)
  function \??\ (S : STD_ULOGIC) return BOOLEAN;

  -- rtl_synthesis off
-- pragma synthesis_off
  function to_string (value : STD_ULOGIC) return STRING;
  function to_string (value : STD_ULOGIC_VECTOR) return STRING;
  function to_string (value : STD_LOGIC_VECTOR) return STRING;

  -- explicitly defined operations

  alias TO_BSTRING is TO_STRING [STD_ULOGIC_VECTOR return STRING];
  alias TO_BINARY_STRING is TO_STRING [STD_ULOGIC_VECTOR return STRING];
  function TO_OSTRING (VALUE : STD_ULOGIC_VECTOR) return STRING;
  alias TO_OCTAL_STRING is TO_OSTRING [STD_ULOGIC_VECTOR return STRING];
  function TO_HSTRING (VALUE : STD_ULOGIC_VECTOR) return STRING;
  alias TO_HEX_STRING is TO_HSTRING [STD_ULOGIC_VECTOR return STRING];

  procedure READ (L : inout LINE; VALUE : out STD_ULOGIC; GOOD : out BOOLEAN);
  procedure READ (L : inout LINE; VALUE : out STD_ULOGIC);

  procedure READ (L : inout LINE; VALUE : out STD_ULOGIC_VECTOR; GOOD : out BOOLEAN);
  procedure READ (L : inout LINE; VALUE : out STD_ULOGIC_VECTOR);

  procedure WRITE (L         : inout LINE; VALUE : in STD_ULOGIC;
                   JUSTIFIED : in    SIDE := right; FIELD : in WIDTH := 0);

  procedure WRITE (L         : inout LINE; VALUE : in STD_ULOGIC_VECTOR;
                   JUSTIFIED : in    SIDE := right; FIELD : in WIDTH := 0);

  alias BREAD is READ [LINE, STD_ULOGIC_VECTOR, BOOLEAN];
  alias BREAD is READ [LINE, STD_ULOGIC_VECTOR];
  alias BINARY_READ is READ [LINE, STD_ULOGIC_VECTOR, BOOLEAN];
  alias BINARY_READ is READ [LINE, STD_ULOGIC_VECTOR];

  procedure OREAD (L : inout LINE; VALUE : out STD_ULOGIC_VECTOR; GOOD : out BOOLEAN);
  procedure OREAD (L : inout LINE; VALUE : out STD_ULOGIC_VECTOR);
  alias OCTAL_READ is OREAD [LINE, STD_ULOGIC_VECTOR, BOOLEAN];
  alias OCTAL_READ is OREAD [LINE, STD_ULOGIC_VECTOR];

  procedure HREAD (L : inout LINE; VALUE : out STD_ULOGIC_VECTOR; GOOD : out BOOLEAN);
  procedure HREAD (L : inout LINE; VALUE : out STD_ULOGIC_VECTOR);
  alias HEX_READ is HREAD [LINE, STD_ULOGIC_VECTOR, BOOLEAN];
  alias HEX_READ is HREAD [LINE, STD_ULOGIC_VECTOR];

  alias BWRITE is WRITE [LINE, STD_ULOGIC_VECTOR, SIDE, WIDTH];
  alias BINARY_WRITE is WRITE [LINE, STD_ULOGIC_VECTOR, SIDE, WIDTH];

  procedure OWRITE (L         : inout LINE; VALUE : in STD_ULOGIC_VECTOR;
                    JUSTIFIED : in    SIDE := right; FIELD : in WIDTH := 0);
  alias OCTAL_WRITE is OWRITE [LINE, STD_ULOGIC_VECTOR, SIDE, WIDTH];

  procedure HWRITE (L         : inout LINE; VALUE : in STD_ULOGIC_VECTOR;
                    JUSTIFIED : in    SIDE := right; FIELD : in WIDTH := 0);
  alias HEX_WRITE is HWRITE [LINE, STD_ULOGIC_VECTOR, SIDE, WIDTH];

  alias TO_BSTRING is TO_STRING [STD_LOGIC_VECTOR return STRING];
  alias TO_BINARY_STRING is TO_STRING [STD_LOGIC_VECTOR return STRING];
  function TO_OSTRING (VALUE : STD_LOGIC_VECTOR) return STRING;
  alias TO_OCTAL_STRING is TO_OSTRING [STD_LOGIC_VECTOR return STRING];
  function TO_HSTRING (VALUE : STD_LOGIC_VECTOR) return STRING;
  alias TO_HEX_STRING is TO_HSTRING [STD_LOGIC_VECTOR return STRING];

  procedure READ (L : inout LINE; VALUE : out STD_LOGIC_VECTOR; GOOD : out BOOLEAN);
  procedure READ (L : inout LINE; VALUE : out STD_LOGIC_VECTOR);

  procedure WRITE (L         : inout LINE; VALUE : in STD_LOGIC_VECTOR;
                   JUSTIFIED : in    SIDE := right; FIELD : in WIDTH := 0);

  alias BREAD is READ [LINE, STD_LOGIC_VECTOR, BOOLEAN];
  alias BREAD is READ [LINE, STD_LOGIC_VECTOR];
  alias BINARY_READ is READ [LINE, STD_LOGIC_VECTOR, BOOLEAN];
  alias BINARY_READ is READ [LINE, STD_LOGIC_VECTOR];

  procedure OREAD (L : inout LINE; VALUE : out STD_LOGIC_VECTOR; GOOD : out BOOLEAN);
  procedure OREAD (L : inout LINE; VALUE : out STD_LOGIC_VECTOR);
  alias OCTAL_READ is OREAD [LINE, STD_LOGIC_VECTOR, BOOLEAN];
  alias OCTAL_READ is OREAD [LINE, STD_LOGIC_VECTOR];

  procedure HREAD (L : inout LINE; VALUE : out STD_LOGIC_VECTOR; GOOD : out BOOLEAN);
  procedure HREAD (L : inout LINE; VALUE : out STD_LOGIC_VECTOR);
  alias HEX_READ is HREAD [LINE, STD_LOGIC_VECTOR, BOOLEAN];
  alias HEX_READ is HREAD [LINE, STD_LOGIC_VECTOR];

  alias BWRITE is WRITE [LINE, STD_LOGIC_VECTOR, SIDE, WIDTH];
  alias BINARY_WRITE is WRITE [LINE, STD_LOGIC_VECTOR, SIDE, WIDTH];

  procedure OWRITE (L         : inout LINE; VALUE : in STD_LOGIC_VECTOR;
                    JUSTIFIED : in    SIDE := right; FIELD : in WIDTH := 0);
  alias OCTAL_WRITE is OWRITE [LINE, STD_LOGIC_VECTOR, SIDE, WIDTH];

  procedure HWRITE (L         : inout LINE; VALUE : in STD_LOGIC_VECTOR;
                    JUSTIFIED : in    SIDE := right; FIELD : in WIDTH := 0);
  alias HEX_WRITE is HWRITE [LINE, STD_LOGIC_VECTOR, SIDE, WIDTH];
  -- rtl_synthesis on
-- pragma synthesis_on
  function maximum (l, r : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR;
  function maximum (l, r : STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR;
  function maximum (l, r : STD_ULOGIC) return STD_ULOGIC;
  function minimum (l, r : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR;
  function minimum (l, r : STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR;
  function minimum (l, r : STD_ULOGIC) return STD_ULOGIC;
end package std_logic_1164_additions;

package body std_logic_1164_additions is
  type stdlogic_table is array(STD_ULOGIC, STD_ULOGIC) of STD_ULOGIC;
  -----------------------------------------------------------------------------
  -- New/updated funcitons for VHDL-200X fast track
  -----------------------------------------------------------------------------
  -- to_01
  -------------------------------------------------------------------
  function TO_01 (s : STD_ULOGIC_VECTOR; xmap : STD_ULOGIC := '0')
    return STD_ULOGIC_VECTOR is
    variable RESULT      : STD_ULOGIC_VECTOR(s'length-1 downto 0);
    variable BAD_ELEMENT : BOOLEAN := false;
    alias XS             : STD_ULOGIC_VECTOR(s'length-1 downto 0) is s;
  begin
    for I in RESULT'range loop
      case XS(I) is
        when '0' | 'L' => RESULT(I)   := '0';
        when '1' | 'H' => RESULT(I)   := '1';
        when others    => BAD_ELEMENT := true;
      end case;
    end loop;
    if BAD_ELEMENT then
      for I in RESULT'range loop
        RESULT(I) := XMAP;              -- standard fixup
      end loop;
    end if;
    return RESULT;
  end function TO_01;
  -------------------------------------------------------------------
  function TO_01 (s : STD_ULOGIC; xmap : STD_ULOGIC := '0')
    return STD_ULOGIC is
  begin
    case s is
      when '0' | 'L' => RETURN '0';
      when '1' | 'H' => RETURN '1';
      when others    => return xmap;
    end case;
  end function TO_01;
  -------------------------------------------------------------------
  function TO_01 (s : BIT_VECTOR; xmap : STD_ULOGIC := '0')
    return STD_ULOGIC_VECTOR is
    variable RESULT : STD_ULOGIC_VECTOR(s'length-1 downto 0);
    alias XS        : BIT_VECTOR(s'length-1 downto 0) is s;
  begin
    for I in RESULT'range loop
      case XS(I) is
        when '0' => RESULT(I) := '0';
        when '1' => RESULT(I) := '1';
      end case;
    end loop;
    return RESULT;
  end function TO_01;
  -------------------------------------------------------------------
  function TO_01 (s : BIT; xmap : STD_ULOGIC := '0')
    return STD_ULOGIC is
  begin
    case s is
      when '0' => RETURN '0';
      when '1' => RETURN '1';
    end case;
  end function TO_01;
-- end Bugzilla issue #148
  -------------------------------------------------------------------

  -------------------------------------------------------------------
  -- overloaded shift operators
  -------------------------------------------------------------------

  -------------------------------------------------------------------
  -- sll
  -------------------------------------------------------------------
  function "sll" (l : STD_LOGIC_VECTOR; r : INTEGER) return STD_LOGIC_VECTOR is
    alias lv        : STD_LOGIC_VECTOR (1 to l'length) is l;
    variable result : STD_LOGIC_VECTOR (1 to l'length) := (others => '0');
  begin
    if r >= 0 then
      result(1 to l'length - r) := lv(r + 1 to l'length);
    else
      result := l srl -r;
    end if;
    return result;
  end function "sll";
  -------------------------------------------------------------------
  function "sll" (l : STD_ULOGIC_VECTOR; r : INTEGER) return STD_ULOGIC_VECTOR is
    alias lv        : STD_ULOGIC_VECTOR (1 to l'length) is l;
    variable result : STD_ULOGIC_VECTOR (1 to l'length) := (others => '0');
  begin
    if r >= 0 then
      result(1 to l'length - r) := lv(r + 1 to l'length);
    else
      result := l srl -r;
    end if;
    return result;
  end function "sll";

  -------------------------------------------------------------------
  -- srl
  -------------------------------------------------------------------
  function "srl" (l : STD_LOGIC_VECTOR; r : INTEGER) return STD_LOGIC_VECTOR is
    alias lv        : STD_LOGIC_VECTOR (1 to l'length) is l;
    variable result : STD_LOGIC_VECTOR (1 to l'length) := (others => '0');
  begin
    if r >= 0 then
      result(r + 1 to l'length) := lv(1 to l'length - r);
    else
      result := l sll -r;
    end if;
    return result;
  end function "srl";
  -------------------------------------------------------------------
  function "srl" (l : STD_ULOGIC_VECTOR; r : INTEGER) return STD_ULOGIC_VECTOR is
    alias lv        : STD_ULOGIC_VECTOR (1 to l'length) is l;
    variable result : STD_ULOGIC_VECTOR (1 to l'length) := (others => '0');
  begin
    if r >= 0 then
      result(r + 1 to l'length) := lv(1 to l'length - r);
    else
      result := l sll -r;
    end if;
    return result;
  end function "srl";

  -------------------------------------------------------------------
  -- rol
  -------------------------------------------------------------------
  function "rol" (l : STD_LOGIC_VECTOR; r : INTEGER) return STD_LOGIC_VECTOR is
    alias lv        : STD_LOGIC_VECTOR (1 to l'length) is l;
    variable result : STD_LOGIC_VECTOR (1 to l'length);
    constant rm     : INTEGER := r mod l'length;
  begin
    if r >= 0 then
      result(1 to l'length - rm)            := lv(rm + 1 to l'length);
      result(l'length - rm + 1 to l'length) := lv(1 to rm);
    else
      result := l ror -r;
    end if;
    return result;
  end function "rol";
  -------------------------------------------------------------------
  function "rol" (l : STD_ULOGIC_VECTOR; r : INTEGER) return STD_ULOGIC_VECTOR is
    alias lv        : STD_ULOGIC_VECTOR (1 to l'length) is l;
    variable result : STD_ULOGIC_VECTOR (1 to l'length);
    constant rm     : INTEGER := r mod l'length;
  begin
    if r >= 0 then
      result(1 to l'length - rm)            := lv(rm + 1 to l'length);
      result(l'length - rm + 1 to l'length) := lv(1 to rm);
    else
      result := l ror -r;
    end if;
    return result;
  end function "rol";

  -------------------------------------------------------------------
  -- ror
  -------------------------------------------------------------------
  function "ror" (l : STD_LOGIC_VECTOR; r : INTEGER) return STD_LOGIC_VECTOR is
    alias lv        : STD_LOGIC_VECTOR (1 to l'length) is l;
    variable result : STD_LOGIC_VECTOR (1 to l'length) := (others => '0');
    constant rm     : INTEGER := r mod l'length;
  begin
    if r >= 0 then
      result(rm + 1 to l'length) := lv(1 to l'length - rm);
      result(1 to rm)            := lv(l'length - rm + 1 to l'length);
    else
      result := l rol -r;
    end if;
    return result;
  end function "ror";
  -------------------------------------------------------------------
  function "ror" (l : STD_ULOGIC_VECTOR; r : INTEGER) return STD_ULOGIC_VECTOR is
    alias lv        : STD_ULOGIC_VECTOR (1 to l'length) is l;
    variable result : STD_ULOGIC_VECTOR (1 to l'length) := (others => '0');
    constant rm     : INTEGER := r mod l'length;
  begin
    if r >= 0 then
      result(rm + 1 to l'length) := lv(1 to l'length - rm);
      result(1 to rm)            := lv(l'length - rm + 1 to l'length);
    else
      result := l rol -r;
    end if;
    return result;
  end function "ror";
  -------------------------------------------------------------------
  -- vector/scalar overloaded logical operators
  -------------------------------------------------------------------

  -------------------------------------------------------------------
  -- and
  -------------------------------------------------------------------
  function "and" (l : STD_LOGIC_VECTOR; r : STD_ULOGIC) return STD_LOGIC_VECTOR is
    alias lv        : STD_LOGIC_VECTOR (1 to l'length) is l;
    variable result : STD_LOGIC_VECTOR (1 to l'length);
  begin
    for i in result'range loop
      result(i) := "and" (lv(i), r);
    end loop;
    return result;
  end function "and";
  -------------------------------------------------------------------
  function "and" (l : STD_ULOGIC_VECTOR; r : STD_ULOGIC) return STD_ULOGIC_VECTOR is
    alias lv        : STD_ULOGIC_VECTOR (1 to l'length) is l;
    variable result : STD_ULOGIC_VECTOR (1 to l'length);
  begin
    for i in result'range loop
      result(i) := "and" (lv(i), r);
    end loop;
    return result;
  end function "and";
  -------------------------------------------------------------------
  function "and" (l : STD_ULOGIC; r : STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR is
    alias rv        : STD_LOGIC_VECTOR (1 to r'length) is r;
    variable result : STD_LOGIC_VECTOR (1 to r'length);
  begin
    for i in result'range loop
      result(i) := "and" (l, rv(i));
    end loop;
    return result;
  end function "and";
  -------------------------------------------------------------------
  function "and" (l : STD_ULOGIC; r : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR is
    alias rv        : STD_ULOGIC_VECTOR (1 to r'length) is r;
    variable result : STD_ULOGIC_VECTOR (1 to r'length);
  begin
    for i in result'range loop
      result(i) := "and" (l, rv(i));
    end loop;
    return result;
  end function "and";

  -------------------------------------------------------------------
  -- nand
  -------------------------------------------------------------------
  function "nand" (l : STD_LOGIC_VECTOR; r : STD_ULOGIC) return STD_LOGIC_VECTOR is
    alias lv        : STD_LOGIC_VECTOR (1 to l'length) is l;
    variable result : STD_LOGIC_VECTOR (1 to l'length);
  begin
    for i in result'range loop
      result(i) := "not"("and" (lv(i), r));
    end loop;
    return result;
  end function "nand";
  -------------------------------------------------------------------
  function "nand" (l : STD_ULOGIC_VECTOR; r : STD_ULOGIC) return STD_ULOGIC_VECTOR is
    alias lv        : STD_ULOGIC_VECTOR (1 to l'length) is l;
    variable result : STD_ULOGIC_VECTOR (1 to l'length);
  begin
    for i in result'range loop
      result(i) := "not"("and" (lv(i), r));
    end loop;
    return result;
  end function "nand";
  -------------------------------------------------------------------
  function "nand" (l : STD_ULOGIC; r : STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR is
    alias rv        : STD_LOGIC_VECTOR (1 to r'length) is r;
    variable result : STD_LOGIC_VECTOR (1 to r'length);
  begin
    for i in result'range loop
      result(i) := "not"("and" (l, rv(i)));
    end loop;
    return result;
  end function "nand";
  -------------------------------------------------------------------
  function "nand" (l : STD_ULOGIC; r : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR is
    alias rv        : STD_ULOGIC_VECTOR (1 to r'length) is r;
    variable result : STD_ULOGIC_VECTOR (1 to r'length);
  begin
    for i in result'range loop
      result(i) := "not"("and" (l, rv(i)));
    end loop;
    return result;
  end function "nand";

  -------------------------------------------------------------------
  -- or
  -------------------------------------------------------------------
  function "or" (l : STD_LOGIC_VECTOR; r : STD_ULOGIC) return STD_LOGIC_VECTOR is
    alias lv        : STD_LOGIC_VECTOR (1 to l'length) is l;
    variable result : STD_LOGIC_VECTOR (1 to l'length);
  begin
    for i in result'range loop
      result(i) := "or" (lv(i), r);
    end loop;
    return result;
  end function "or";
  -------------------------------------------------------------------
  function "or" (l : STD_ULOGIC_VECTOR; r : STD_ULOGIC) return STD_ULOGIC_VECTOR is
    alias lv        : STD_ULOGIC_VECTOR (1 to l'length) is l;
    variable result : STD_ULOGIC_VECTOR (1 to l'length);
  begin
    for i in result'range loop
      result(i) := "or" (lv(i), r);
    end loop;
    return result;
  end function "or";
  -------------------------------------------------------------------
  function "or" (l : STD_ULOGIC; r : STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR is
    alias rv        : STD_LOGIC_VECTOR (1 to r'length) is r;
    variable result : STD_LOGIC_VECTOR (1 to r'length);
  begin
    for i in result'range loop
      result(i) := "or" (l, rv(i));
    end loop;
    return result;
  end function "or";
  -------------------------------------------------------------------
  function "or" (l : STD_ULOGIC; r : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR is
    alias rv        : STD_ULOGIC_VECTOR (1 to r'length) is r;
    variable result : STD_ULOGIC_VECTOR (1 to r'length);
  begin
    for i in result'range loop
      result(i) := "or" (l, rv(i));
    end loop;
    return result;
  end function "or";

  -------------------------------------------------------------------
  -- nor
  -------------------------------------------------------------------
  function "nor" (l : STD_LOGIC_VECTOR; r : STD_ULOGIC) return STD_LOGIC_VECTOR is
    alias lv        : STD_LOGIC_VECTOR (1 to l'length) is l;
    variable result : STD_LOGIC_VECTOR (1 to l'length);
  begin
    for i in result'range loop
      result(i) := "not"("or" (lv(i), r));
    end loop;
    return result;
  end function "nor";
  -------------------------------------------------------------------
  function "nor" (l : STD_ULOGIC_VECTOR; r : STD_ULOGIC) return STD_ULOGIC_VECTOR is
    alias lv        : STD_ULOGIC_VECTOR (1 to l'length) is l;
    variable result : STD_ULOGIC_VECTOR (1 to l'length);
  begin
    for i in result'range loop
      result(i) := "not"("or" (lv(i), r));
    end loop;
    return result;
  end function "nor";
  -------------------------------------------------------------------
  function "nor" (l : STD_ULOGIC; r : STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR is
    alias rv        : STD_LOGIC_VECTOR (1 to r'length) is r;
    variable result : STD_LOGIC_VECTOR (1 to r'length);
  begin
    for i in result'range loop
      result(i) := "not"("or" (l, rv(i)));
    end loop;
    return result;
  end function "nor";
  -------------------------------------------------------------------
  function "nor" (l : STD_ULOGIC; r : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR is
    alias rv        : STD_ULOGIC_VECTOR (1 to r'length) is r;
    variable result : STD_ULOGIC_VECTOR (1 to r'length);
  begin
    for i in result'range loop
      result(i) := "not"("or" (l, rv(i)));
    end loop;
    return result;
  end function "nor";

  -------------------------------------------------------------------
  -- xor
  -------------------------------------------------------------------
  function "xor" (l : STD_LOGIC_VECTOR; r : STD_ULOGIC) return STD_LOGIC_VECTOR is
    alias lv        : STD_LOGIC_VECTOR (1 to l'length) is l;
    variable result : STD_LOGIC_VECTOR (1 to l'length);
  begin
    for i in result'range loop
      result(i) := "xor" (lv(i), r);
    end loop;
    return result;
  end function "xor";
  -------------------------------------------------------------------
  function "xor" (l : STD_ULOGIC_VECTOR; r : STD_ULOGIC) return STD_ULOGIC_VECTOR is
    alias lv        : STD_ULOGIC_VECTOR (1 to l'length) is l;
    variable result : STD_ULOGIC_VECTOR (1 to l'length);
  begin
    for i in result'range loop
      result(i) := "xor" (lv(i), r);
    end loop;
    return result;
  end function "xor";
  -------------------------------------------------------------------
  function "xor" (l : STD_ULOGIC; r : STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR is
    alias rv        : STD_LOGIC_VECTOR (1 to r'length) is r;
    variable result : STD_LOGIC_VECTOR (1 to r'length);
  begin
    for i in result'range loop
      result(i) := "xor" (l, rv(i));
    end loop;
    return result;
  end function "xor";
  -------------------------------------------------------------------
  function "xor" (l : STD_ULOGIC; r : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR is
    alias rv        : STD_ULOGIC_VECTOR (1 to r'length) is r;
    variable result : STD_ULOGIC_VECTOR (1 to r'length);
  begin
    for i in result'range loop
      result(i) := "xor" (l, rv(i));
    end loop;
    return result;
  end function "xor";

  -------------------------------------------------------------------
  -- xnor
  -------------------------------------------------------------------
  function "xnor" (l : STD_LOGIC_VECTOR; r : STD_ULOGIC) return STD_LOGIC_VECTOR is
    alias lv        : STD_LOGIC_VECTOR (1 to l'length) is l;
    variable result : STD_LOGIC_VECTOR (1 to l'length);
  begin
    for i in result'range loop
      result(i) := "not"("xor" (lv(i), r));
    end loop;
    return result;
  end function "xnor";
  -------------------------------------------------------------------
  function "xnor" (l : STD_ULOGIC_VECTOR; r : STD_ULOGIC) return STD_ULOGIC_VECTOR is
    alias lv        : STD_ULOGIC_VECTOR (1 to l'length) is l;
    variable result : STD_ULOGIC_VECTOR (1 to l'length);
  begin
    for i in result'range loop
      result(i) := "not"("xor" (lv(i), r));
    end loop;
    return result;
  end function "xnor";
  -------------------------------------------------------------------
  function "xnor" (l : STD_ULOGIC; r : STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR is
    alias rv        : STD_LOGIC_VECTOR (1 to r'length) is r;
    variable result : STD_LOGIC_VECTOR (1 to r'length);
  begin
    for i in result'range loop
      result(i) := "not"("xor" (l, rv(i)));
    end loop;
    return result;
  end function "xnor";
  -------------------------------------------------------------------
  function "xnor" (l : STD_ULOGIC; r : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR is
    alias rv        : STD_ULOGIC_VECTOR (1 to r'length) is r;
    variable result : STD_ULOGIC_VECTOR (1 to r'length);
  begin
    for i in result'range loop
      result(i) := "not"("xor" (l, rv(i)));
    end loop;
    return result;
  end function "xnor";

  -------------------------------------------------------------------
  -- vector-reduction functions
  -------------------------------------------------------------------

  -------------------------------------------------------------------
  -- and
  -------------------------------------------------------------------
  function and_reduce (l : STD_LOGIC_VECTOR) return STD_ULOGIC is
  begin
    return and_reduce (to_StdULogicVector (l));
  end function and_reduce;
  -------------------------------------------------------------------
  function and_reduce (l : STD_ULOGIC_VECTOR) return STD_ULOGIC is
    variable result : STD_ULOGIC := '1';
  begin
    for i in l'reverse_range loop
      result := (l(i) and result);
    end loop;
    return result;
  end function and_reduce;

  -------------------------------------------------------------------
  -- nand
  -------------------------------------------------------------------
  function nand_reduce (l : STD_LOGIC_VECTOR) return STD_ULOGIC is
  begin
    return not (and_reduce(to_StdULogicVector(l)));
  end function nand_reduce;
  -------------------------------------------------------------------
  function nand_reduce (l : STD_ULOGIC_VECTOR) return STD_ULOGIC is
  begin
    return  not (and_reduce(l));
  end function nand_reduce;

  -------------------------------------------------------------------
  -- or
  -------------------------------------------------------------------
  function or_reduce (l : STD_LOGIC_VECTOR) return STD_ULOGIC is
  begin
    return or_reduce (to_StdULogicVector (l));
  end function or_reduce;
  -------------------------------------------------------------------
  function or_reduce (l : STD_ULOGIC_VECTOR) return STD_ULOGIC is
    variable result : STD_ULOGIC := '0';
  begin
    for i in l'reverse_range loop
      result := (l(i) or result);
    end loop;
    return result;
  end function or_reduce;

  -------------------------------------------------------------------
  -- nor
  -------------------------------------------------------------------
  function nor_reduce (l : STD_LOGIC_VECTOR) return STD_ULOGIC is
  begin
    return "not"(or_reduce(To_StdULogicVector(l)));
  end function nor_reduce;
  -------------------------------------------------------------------
  function nor_reduce (l : STD_ULOGIC_VECTOR) return STD_ULOGIC is
  begin
    return "not"(or_reduce(l));
  end function nor_reduce;

  -------------------------------------------------------------------
  -- xor
  -------------------------------------------------------------------
  function xor_reduce (l : STD_LOGIC_VECTOR) return STD_ULOGIC is
  begin
    return xor_reduce (to_StdULogicVector (l));
  end function xor_reduce;
  -------------------------------------------------------------------
  function xor_reduce (l : STD_ULOGIC_VECTOR) return STD_ULOGIC is
    variable result : STD_ULOGIC := '0';
  begin
    for i in l'reverse_range loop
      result := (l(i) xor result);
    end loop;
    return result;
  end function xor_reduce;

  -------------------------------------------------------------------
  -- xnor
  -------------------------------------------------------------------
  function xnor_reduce (l : STD_LOGIC_VECTOR) return STD_ULOGIC is
  begin
    return "not"(xor_reduce(To_StdULogicVector(l)));
  end function xnor_reduce;
  -------------------------------------------------------------------
  function xnor_reduce (l : STD_ULOGIC_VECTOR) return STD_ULOGIC is
  begin
    return "not"(xor_reduce(l));
  end function xnor_reduce;
  -- %%% End "remove the following functions"

    constant match_logic_table : stdlogic_table := (
    -----------------------------------------------------
    -- U    X    0    1    Z    W    L    H    -       |   |
    -----------------------------------------------------
    ('U', 'U', 'U', 'U', 'U', 'U', 'U', 'U', '1'),  -- | U |
    ('U', 'X', 'X', 'X', 'X', 'X', 'X', 'X', '1'),  -- | X |
    ('U', 'X', '1', '0', 'X', 'X', '1', '0', '1'),  -- | 0 |
    ('U', 'X', '0', '1', 'X', 'X', '0', '1', '1'),  -- | 1 |
    ('U', 'X', 'X', 'X', 'X', 'X', 'X', 'X', '1'),  -- | Z |
    ('U', 'X', 'X', 'X', 'X', 'X', 'X', 'X', '1'),  -- | W |
    ('U', 'X', '1', '0', 'X', 'X', '1', '0', '1'),  -- | L |
    ('U', 'X', '0', '1', 'X', 'X', '0', '1', '1'),  -- | H |
    ('1', '1', '1', '1', '1', '1', '1', '1', '1')   -- | - |
    );

  -------------------------------------------------------------------
  -- ?= functions, Similar to "std_match", but returns "std_ulogic".
  -------------------------------------------------------------------
  -- %%% FUNCTION "?=" ( l, r : std_ulogic ) RETURN std_ulogic IS
  function \?=\ (l, r : STD_ULOGIC) return STD_ULOGIC is
  begin
    return match_logic_table (l, r);
  end function \?=\;
  -- %%% END FUNCTION "?=";
  -------------------------------------------------------------------
  -- %%% FUNCTION "?=" ( l, r : std_logic_vector ) RETURN std_ulogic IS
  function \?=\ (l, r : STD_LOGIC_VECTOR) return STD_ULOGIC is
    alias lv                 : STD_LOGIC_VECTOR(1 to l'length) is l;
    alias rv                 : STD_LOGIC_VECTOR(1 to r'length) is r;
    variable result, result1 : STD_ULOGIC;  -- result
  begin
    -- Logically identical to an "=" operator.
    if ((l'length < 1) and (r'length < 1)) then
      -- VHDL-2008 LRM 9.2.3 Two NULL arrays of the same type are equal
      return '1';
    elsif lv'length /= rv'length then
      -- Two arrays of different lengths are false
      return '0';
    else
      result := '1';
      for i in lv'low to lv'high loop
        result1 := match_logic_table(lv(i), rv(i));
        result := result and result1;
      end loop;
      return result;
    end if;
  end function \?=\;
  -- %%% END FUNCTION "?=";
  -------------------------------------------------------------------
  -- %%% FUNCTION "?=" ( l, r : std_ulogic_vector ) RETURN std_ulogic IS
  function \?=\ (l, r : STD_ULOGIC_VECTOR) return STD_ULOGIC is
    alias lv                 : STD_ULOGIC_VECTOR(1 to l'length) is l;
    alias rv                 : STD_ULOGIC_VECTOR(1 to r'length) is r;
    variable result, result1 : STD_ULOGIC;
  begin
    -- Logically identical to an "=" operator.
    if ((l'length < 1) and (r'length < 1)) then
      -- VHDL-2008 LRM 9.2.3 Two NULL arrays of the same type are equal
      return '1';
    elsif lv'length /= rv'length then
      -- Two arrays of different lengths are false
      return '0';
    else
      result := '1';
      for i in lv'low to lv'high loop
        result1 := match_logic_table(lv(i), rv(i));
        result := result and result1;
      end loop;
      return result;
    end if;
  end function \?=\;
  -- %%% END FUNCTION "?=";
  -- %%% FUNCTION "?/=" ( l, r : std_ulogic ) RETURN std_ulogic is
  function \?/=\ (l, r : STD_ULOGIC) return STD_ULOGIC is
  begin
    return not \?=\ (l, r);
  end function \?/=\;
  -- %%% END FUNCTION "?/=";
  -- %%% FUNCTION "?/=" ( l, r : std_logic_vector ) RETURN std_ulogic is
  function \?/=\ (l, r : STD_LOGIC_VECTOR) return STD_ULOGIC is
  begin
    return not \?=\ (l, r);
  end function \?/=\;
  -- %%% END FUNCTION "?/=";
  -- %%% FUNCTION "?/=" ( l, r : std_ulogic_vector ) RETURN std_ulogic is
  function \?/=\ (l, r : STD_ULOGIC_VECTOR) return STD_ULOGIC is
  begin
    return not \?=\ (l, r);
  end function \?/=\;
  -- %%% END FUNCTION "?/=";

  -- Table for the ?< function (Section 9.2.3)
  constant qlt : stdlogic_table := (
    -----------------------------------------------------
    -- U    X    0    1    Z    W    L    H    -       |   |
    -----------------------------------------------------
    ('U', 'U', 'U', 'U', 'U', 'U', 'U', 'U', 'X'), -- | U |
    ('U', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X'), -- | X |
    ('U', 'X', '0', '1', 'X', 'X', '0', '1', 'X'), -- | 0 |
    ('U', 'X', '0', '0', 'X', 'X', '0', '0', 'X'), -- | 1 |
    ('U', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X'), -- | Z |
    ('U', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X'), -- | W |
    ('U', 'X', '0', '1', 'X', 'X', '0', '1', 'X'), -- | L |
    ('U', 'X', '0', '0', 'X', 'X', '0', '0', 'X'), -- | H |
    ('X', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X')  -- | - |
    );

  -- %%% FUNCTION "?>" ( l, r : std_ulogic ) RETURN std_ulogic is
  function \?>\ (l, r : STD_ULOGIC) return STD_ULOGIC is
  begin
    return not (qlt (l, r) or match_logic_table (l,r));
  end function \?>\;
  -- %%% END FUNCTION "?>";

  -- %%% FUNCTION "?>=" ( l, r : std_ulogic ) RETURN std_ulogic is
  function \?>=\ (l, r : STD_ULOGIC) return STD_ULOGIC is
  begin
    return not qlt (l, r);
  end function \?>=\;
  -- %%% END FUNCTION "?>=";

  -- %%% FUNCTION "?<" ( l, r : std_ulogic ) RETURN std_ulogic is
  function \?<\ (l, r : STD_ULOGIC) return STD_ULOGIC is
  begin
    return qlt (l, r);
  end function \?<\;
  -- %%% END FUNCTION "?<";

  -- %%% FUNCTION "?<=" ( l, r : std_ulogic ) RETURN std_ulogic is
  function \?<=\ (l, r : STD_ULOGIC) return STD_ULOGIC is
  begin
    return qlt (l, r) or match_logic_table (l,r);
  end function \?<=\;
  -- %%% END FUNCTION "?<=";

  -- "??" operator, converts a std_ulogic to a boolean.
-- %%% FUNCTION "??"
  function \??\ (S : STD_ULOGIC) return BOOLEAN is
  begin
    return S = '1' or S = 'H';
  end function \??\;
-- %%% END FUNCTION "??";

  -- rtl_synthesis off
-- pragma synthesis_off
  -----------------------------------------------------------------------------
  -- This section copied from "std_logic_textio"
  -----------------------------------------------------------------------------
  -- Type and constant definitions used to map STD_ULOGIC values
  -- into/from character values.
  type MVL9plus is ('U', 'X', '0', '1', 'Z', 'W', 'L', 'H', '-', error);
  type char_indexed_by_MVL9 is array (STD_ULOGIC) of CHARACTER;
  type MVL9_indexed_by_char is array (CHARACTER) of STD_ULOGIC;
  type MVL9plus_indexed_by_char is array (CHARACTER) of MVL9plus;
  constant MVL9_to_char : char_indexed_by_MVL9 := "UX01ZWLH-";
  constant char_to_MVL9 : MVL9_indexed_by_char :=
    ('U' => 'U', 'X' => 'X', '0' => '0', '1' => '1', 'Z' => 'Z',
     'W' => 'W', 'L' => 'L', 'H' => 'H', '-' => '-', others => 'U');
  constant char_to_MVL9plus : MVL9plus_indexed_by_char :=
    ('U' => 'U', 'X' => 'X', '0' => '0', '1' => '1', 'Z' => 'Z',
     'W' => 'W', 'L' => 'L', 'H' => 'H', '-' => '-', others => error);

  constant NBSP : CHARACTER      := CHARACTER'val(160);  -- space character
  constant NUS  : STRING(2 to 1) := (others => ' ');     -- null STRING

  -- purpose: Skips white space
  procedure skip_whitespace (
    L : inout LINE) is
    variable readOk : BOOLEAN;
    variable c : CHARACTER;
  begin
    while L /= null and L.all'length /= 0 loop
      if (L.all(1) = ' ' or L.all(1) = NBSP or L.all(1) = HT) then
        read (l, c, readOk);
      else
        exit;
      end if;
    end loop;
  end procedure skip_whitespace;

  procedure READ (L    : inout LINE; VALUE : out STD_ULOGIC;
                  GOOD : out   BOOLEAN) is
    variable c      : CHARACTER;
    variable readOk : BOOLEAN;
  begin
    VALUE := 'U';                       -- initialize to a "U"
    Skip_whitespace (L);
    read (l, c, readOk);
    if not readOk then
      good := false;
    else
      if char_to_MVL9plus(c) = error then
        good := false;
      else
        VALUE := char_to_MVL9(c);
        good  := true;
      end if;
    end if;
  end procedure READ;

  procedure READ (L    : inout LINE; VALUE : out STD_ULOGIC_VECTOR;
                  GOOD : out   BOOLEAN) is
    variable m      : STD_ULOGIC;
    variable c      : CHARACTER;
    variable mv     : STD_ULOGIC_VECTOR(0 to VALUE'length-1);
    variable readOk : BOOLEAN;
    variable i      : INTEGER;
    variable lastu  : BOOLEAN := false;       -- last character was an "_"
  begin
    VALUE := (VALUE'range => 'U'); -- initialize to a "U"
    Skip_whitespace (L);
    if VALUE'length > 0 then
      read (l, c, readOk);
      i := 0;
      good := false;
      while i < VALUE'length loop
        if not readOk then     -- Bail out if there was a bad read
          return;
        elsif c = '_' then
          if i = 0 then     -- Begins with an "_"
            return;
          elsif lastu then                -- "__" detected
            return;
          else
            lastu := true;
          end if;
        elsif (char_to_MVL9plus(c) = error) then          -- Illegal character
          return;
        else
          mv(i) := char_to_MVL9(c);
          i := i + 1;
          if i > mv'high then             -- reading done
            good := true;
            VALUE := mv;
            return;
          end if;
          lastu := false;
        end if;
        read(L, c, readOk);
      end loop;
    else
      good := true;                   -- read into a null array
    end if;
  end procedure READ;

  procedure READ (L : inout LINE; VALUE : out STD_ULOGIC) is
    variable c      : CHARACTER;
    variable readOk : BOOLEAN;
  begin
    VALUE := 'U';                       -- initialize to a "U"
    Skip_whitespace (L);
    read (l, c, readOk);
    if not readOk then
      report "STD_LOGIC_1164.READ(STD_ULOGIC) "
        & "End of string encountered"
        severity error;
      return;
    elsif char_to_MVL9plus(c) = error then
      report
        "STD_LOGIC_1164.READ(STD_ULOGIC) Error: Character '" &
        c & "' read, expected STD_ULOGIC literal."
        severity error;
    else
      VALUE := char_to_MVL9(c);
    end if;
  end procedure READ;

  procedure READ (L : inout LINE; VALUE : out STD_ULOGIC_VECTOR) is
    variable m      : STD_ULOGIC;
    variable c      : CHARACTER;
    variable readOk : BOOLEAN;
    variable mv     : STD_ULOGIC_VECTOR(0 to VALUE'length-1);
    variable i      : INTEGER;
    variable lastu  : BOOLEAN := false;       -- last character was an "_"
  begin
    VALUE := (VALUE'range => 'U'); -- initialize to a "U"
    Skip_whitespace (L);
    if VALUE'length > 0 then            -- non Null input string
      read (l, c, readOk);
      i := 0;
      while i < VALUE'length loop
        if readOk = false then              -- Bail out if there was a bad read
          report "STD_LOGIC_1164.READ(STD_ULOGIC_VECTOR) "
            & "End of string encountered"
            severity error;
          return;
        elsif c = '_' then
          if i = 0 then
            report "STD_LOGIC_1164.READ(STD_ULOGIC_VECTOR) "
              & "String begins with an ""_""" severity error;
            return;
          elsif lastu then
            report "STD_LOGIC_1164.READ(STD_ULOGIC_VECTOR) "
              & "Two underscores detected in input string ""__"""
              severity error;
            return;
          else
            lastu := true;
          end if;
        elsif c = ' ' or c = NBSP or c = HT then  -- reading done.
          report "STD_LOGIC_1164.READ(STD_ULOGIC_VECTOR) "
            & "Short read, Space encounted in input string"
            severity error;
          return;
        elsif char_to_MVL9plus(c) = error then
          report "STD_LOGIC_1164.READ(STD_ULOGIC_VECTOR) "
            & "Error: Character '" &
            c & "' read, expected STD_ULOGIC literal."
            severity error;
          return;
        else
          mv(i) := char_to_MVL9(c);
          i := i + 1;
          if i > mv'high then
            VALUE := mv;
            return;
          end if;
          lastu := false;
        end if;
        read(L, c, readOk);
      end loop;
    end if;
  end procedure READ;

  procedure WRITE (L         : inout LINE; VALUE : in STD_ULOGIC;
                   JUSTIFIED : in    SIDE := right; FIELD : in WIDTH := 0) is
  begin
    write(l, MVL9_to_char(VALUE), justified, field);
  end procedure WRITE;

  procedure WRITE (L         : inout LINE; VALUE : in STD_ULOGIC_VECTOR;
                   JUSTIFIED : in    SIDE := right; FIELD : in WIDTH := 0) is
    variable s : STRING(1 to VALUE'length);
    variable m : STD_ULOGIC_VECTOR(1 to VALUE'length) := VALUE;
  begin
    for i in 1 to VALUE'length loop
      s(i) := MVL9_to_char(m(i));
    end loop;
    write(l, s, justified, field);
  end procedure WRITE;

  -- Read and Write procedures for STD_LOGIC_VECTOR

  procedure READ (L    : inout LINE; VALUE : out STD_LOGIC_VECTOR;
                  GOOD : out   BOOLEAN) is
    variable ivalue : STD_ULOGIC_VECTOR (VALUE'range);
  begin
    READ (L => L, VALUE => ivalue, GOOD => GOOD);
    VALUE := to_stdlogicvector (ivalue);
  end procedure READ;

  procedure READ (L : inout LINE; VALUE : out STD_LOGIC_VECTOR) is
    variable ivalue : STD_ULOGIC_VECTOR (VALUE'range);
  begin
    READ (L => L, VALUE => ivalue);
    VALUE := to_stdlogicvector (ivalue);
  end procedure READ;

  procedure WRITE (L         : inout LINE; VALUE : in STD_LOGIC_VECTOR;
                   JUSTIFIED : in    SIDE := right; FIELD : in WIDTH := 0) is
    variable s : STRING(1 to VALUE'length);
    variable m : STD_LOGIC_VECTOR(1 to VALUE'length) := VALUE;
  begin
    for i in 1 to VALUE'length loop
      s(i) := MVL9_to_char(m(i));
    end loop;
    write(L, s, justified, field);
  end procedure WRITE;

  -----------------------------------------------------------------------
  -- Alias for bread and bwrite are provided with call out the read and
  -- write functions.
  -----------------------------------------------------------------------

  -- Hex Read and Write procedures for STD_ULOGIC_VECTOR.
  -- Modified from the original to be more forgiving.

  procedure Char2QuadBits (C           :     CHARACTER;
                           RESULT      : out STD_ULOGIC_VECTOR(3 downto 0);
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
      when 'Z'       => result := "ZZZZ"; good := true;
      when 'X'       => result := "XXXX"; good := true;
      when others =>
        assert not ISSUE_ERROR
          report
          "STD_LOGIC_1164.HREAD Read a '" & c &
          "', expected a Hex character (0-F)."
          severity error;
        good := false;
    end case;
  end procedure Char2QuadBits;

  procedure HREAD (L    : inout LINE; VALUE : out STD_ULOGIC_VECTOR;
                   GOOD : out   BOOLEAN) is
    variable ok  : BOOLEAN;
    variable c   : CHARACTER;
    constant ne  : INTEGER := (VALUE'length+3)/4;
    constant pad : INTEGER := ne*4 - VALUE'length;
    variable sv  : STD_ULOGIC_VECTOR(0 to ne*4 - 1);
    variable i   : INTEGER;
    variable lastu  : BOOLEAN := false;       -- last character was an "_"
  begin
    VALUE := (VALUE'range => 'U'); -- initialize to a "U"
    Skip_whitespace (L);
    if VALUE'length > 0 then
      read (l, c, ok);
      i := 0;
      while i < ne loop
        -- Bail out if there was a bad read
        if not ok then
          good := false;
          return;
        elsif c = '_' then
          if i = 0 then
            good := false;                -- Begins with an "_"
            return;
          elsif lastu then
            good := false;                -- "__" detected
            return;
          else
            lastu := true;
          end if;
        else
          Char2QuadBits(c, sv(4*i to 4*i+3), ok, false);
          if not ok then
            good := false;
            return;
          end if;
          i := i + 1;
          lastu := false;
        end if;
        if i < ne then
          read(L, c, ok);
        end if;
      end loop;
      if or_reduce (sv (0 to pad-1)) = '1' then  -- %%% replace with "or"
        good := false;                           -- vector was truncated.
      else
        good  := true;
        VALUE := sv (pad to sv'high);
      end if;
    else
      good := true;                     -- Null input string, skips whitespace
    end if;
  end procedure HREAD;

  procedure HREAD (L : inout LINE; VALUE : out STD_ULOGIC_VECTOR) is
    variable ok  : BOOLEAN;
    variable c   : CHARACTER;
    constant ne  : INTEGER := (VALUE'length+3)/4;
    constant pad : INTEGER := ne*4 - VALUE'length;
    variable sv  : STD_ULOGIC_VECTOR(0 to ne*4 - 1);
    variable i   : INTEGER;
    variable lastu  : BOOLEAN := false;       -- last character was an "_"
  begin
    VALUE := (VALUE'range => 'U'); -- initialize to a "U"
    Skip_whitespace (L);
    if VALUE'length > 0 then           -- non Null input string
      read (l, c, ok);
      i := 0;
      while i < ne loop
        -- Bail out if there was a bad read
        if not ok then
          report "STD_LOGIC_1164.HREAD "
            & "End of string encountered"
            severity error;
          return;
        end if;
        if c = '_' then
          if i = 0 then
            report "STD_LOGIC_1164.HREAD "
              & "String begins with an ""_""" severity error;
            return;
          elsif lastu then
            report "STD_LOGIC_1164.HREAD "
              & "Two underscores detected in input string ""__"""
              severity error;
            return;
          else
            lastu := true;
          end if;
        else
          Char2QuadBits(c, sv(4*i to 4*i+3), ok, true);
          if not ok then
            return;
          end if;
          i := i + 1;
          lastu := false;
        end if;
        if i < ne then
          read(L, c, ok);
        end if;
      end loop;
      if or_reduce (sv (0 to pad-1)) = '1' then  -- %%% replace with "or"
        report "STD_LOGIC_1164.HREAD Vector truncated"
          severity error;
      else
        VALUE := sv (pad to sv'high);
      end if;
    end if;
  end procedure HREAD;

  procedure HWRITE (L         : inout LINE; VALUE : in STD_ULOGIC_VECTOR;
                    JUSTIFIED : in    SIDE := right; FIELD : in WIDTH := 0) is
  begin
    write (L, to_hstring (VALUE), JUSTIFIED, FIELD);
  end procedure HWRITE;


  -- Octal Read and Write procedures for STD_ULOGIC_VECTOR.
  -- Modified from the original to be more forgiving.

  procedure Char2TriBits (C           :     CHARACTER;
                          RESULT      : out STD_ULOGIC_VECTOR(2 downto 0);
                          GOOD        : out BOOLEAN;
                          ISSUE_ERROR : in  BOOLEAN) is
  begin
    case c is
      when '0' => result := o"0"; good := true;
      when '1' => result := o"1"; good := true;
      when '2' => result := o"2"; good := true;
      when '3' => result := o"3"; good := true;
      when '4' => result := o"4"; good := true;
      when '5' => result := o"5"; good := true;
      when '6' => result := o"6"; good := true;
      when '7' => result := o"7"; good := true;
      when 'Z' => result := "ZZZ"; good := true;
      when 'X' => result := "XXX"; good := true;
      when others =>
        assert not ISSUE_ERROR
          report
          "STD_LOGIC_1164.OREAD Error: Read a '" & c &
          "', expected an Octal character (0-7)."
          severity error;
        good := false;
    end case;
  end procedure Char2TriBits;

  procedure OREAD (L    : inout LINE; VALUE : out STD_ULOGIC_VECTOR;
                   GOOD : out   BOOLEAN) is
    variable ok  : BOOLEAN;
    variable c   : CHARACTER;
    constant ne  : INTEGER := (VALUE'length+2)/3;
    constant pad : INTEGER := ne*3 - VALUE'length;
    variable sv  : STD_ULOGIC_VECTOR(0 to ne*3 - 1);
    variable i   : INTEGER;
    variable lastu  : BOOLEAN := false;       -- last character was an "_"
  begin
    VALUE := (VALUE'range => 'U'); -- initialize to a "U"
    Skip_whitespace (L);
    if VALUE'length > 0 then
      read (l, c, ok);
      i := 0;
      while i < ne loop
        -- Bail out if there was a bad read
        if not ok then
          good := false;
          return;
        elsif c = '_' then
          if i = 0 then
            good := false;                -- Begins with an "_"
            return;
          elsif lastu then
            good := false;                -- "__" detected
            return;
          else
            lastu := true;
          end if;
        else
          Char2TriBits(c, sv(3*i to 3*i+2), ok, false);
          if not ok then
            good := false;
            return;
          end if;
          i := i + 1;
          lastu := false;
        end if;
        if i < ne then
          read(L, c, ok);
        end if;
      end loop;
      if or_reduce (sv (0 to pad-1)) = '1' then  -- %%% replace with "or"
        good := false;                           -- vector was truncated.
      else
        good  := true;
        VALUE := sv (pad to sv'high);
      end if;
    else
      good := true;                  -- read into a null array
    end if;
  end procedure OREAD;

  procedure OREAD (L : inout LINE; VALUE : out STD_ULOGIC_VECTOR) is
    variable c   : CHARACTER;
    variable ok  : BOOLEAN;
    constant ne  : INTEGER := (VALUE'length+2)/3;
    constant pad : INTEGER := ne*3 - VALUE'length;
    variable sv  : STD_ULOGIC_VECTOR(0 to ne*3 - 1);
    variable i   : INTEGER;
    variable lastu  : BOOLEAN := false;       -- last character was an "_"
  begin
    VALUE := (VALUE'range => 'U'); -- initialize to a "U"
    Skip_whitespace (L);
    if VALUE'length > 0 then
      read (l, c, ok);
      i := 0;
      while i < ne loop
        -- Bail out if there was a bad read
        if not ok then
          report "STD_LOGIC_1164.OREAD "
            & "End of string encountered"
            severity error;
          return;
        elsif c = '_' then
          if i = 0 then
            report "STD_LOGIC_1164.OREAD "
              & "String begins with an ""_""" severity error;
            return;
          elsif lastu then
            report "STD_LOGIC_1164.OREAD "
              & "Two underscores detected in input string ""__"""
              severity error;
            return;
          else
            lastu := true;
          end if;
        else
          Char2TriBits(c, sv(3*i to 3*i+2), ok, true);
          if not ok then
            return;
          end if;
          i := i + 1;
          lastu := false;
        end if;
        if i < ne then
          read(L, c, ok);
        end if;
      end loop;
      if or_reduce (sv (0 to pad-1)) = '1' then  -- %%% replace with "or"
        report "STD_LOGIC_1164.OREAD Vector truncated"
          severity error;
      else
        VALUE := sv (pad to sv'high);
      end if;
    end if;
  end procedure OREAD;

  procedure OWRITE (L         : inout LINE; VALUE : in STD_ULOGIC_VECTOR;
                    JUSTIFIED : in    SIDE := right; FIELD : in WIDTH := 0) is
  begin
    write (L, to_ostring(VALUE), JUSTIFIED, FIELD);
  end procedure OWRITE;

  -- Hex Read and Write procedures for STD_LOGIC_VECTOR

  procedure HREAD (L    : inout LINE; VALUE : out STD_LOGIC_VECTOR;
                   GOOD : out   BOOLEAN) is
    variable ivalue : STD_ULOGIC_VECTOR (VALUE'range);
  begin
    HREAD (L => L, VALUE => ivalue, GOOD => GOOD);
    VALUE := to_stdlogicvector (ivalue);
  end procedure HREAD;

  procedure HREAD (L : inout LINE; VALUE : out STD_LOGIC_VECTOR) is
    variable ivalue : STD_ULOGIC_VECTOR (VALUE'range);
  begin
    HREAD (L => L, VALUE => ivalue);
    VALUE := to_stdlogicvector (ivalue);
  end procedure HREAD;

  procedure HWRITE (L         : inout LINE; VALUE : in STD_LOGIC_VECTOR;
                    JUSTIFIED : in    SIDE := right; FIELD : in WIDTH := 0) is
  begin
    write (L, to_hstring(VALUE), JUSTIFIED, FIELD);
  end procedure HWRITE;

  -- Octal Read and Write procedures for STD_LOGIC_VECTOR

  procedure OREAD (L    : inout LINE; VALUE : out STD_LOGIC_VECTOR;
                   GOOD : out   BOOLEAN) is
    variable ivalue : STD_ULOGIC_VECTOR (VALUE'range);
  begin
    OREAD (L => L, VALUE => ivalue, GOOD => GOOD);
    VALUE := to_stdlogicvector (ivalue);
  end procedure OREAD;

  procedure OREAD (L : inout LINE; VALUE : out STD_LOGIC_VECTOR) is
    variable ivalue : STD_ULOGIC_VECTOR (VALUE'range);
  begin
    OREAD (L => L, VALUE => ivalue);
    VALUE := to_stdlogicvector (ivalue);
  end procedure OREAD;

  procedure OWRITE (L         : inout LINE; VALUE : in STD_LOGIC_VECTOR;
                    JUSTIFIED : in    SIDE := right; FIELD : in WIDTH := 0) is
  begin
    write (L, to_ostring(VALUE), JUSTIFIED, FIELD);
  end procedure OWRITE;

  -----------------------------------------------------------------------------
  -- New string functions for vhdl-200x fast track
  -----------------------------------------------------------------------------
  function to_string (value     : STD_ULOGIC) return STRING is
    variable result : STRING (1 to 1);
  begin
    result (1) := MVL9_to_char (value);
    return result;
  end function to_string;
  -------------------------------------------------------------------
  -- TO_STRING (an alias called "to_bstring" is provide)
  -------------------------------------------------------------------
  function to_string (value     : STD_ULOGIC_VECTOR) return STRING is
    alias ivalue    : STD_ULOGIC_VECTOR(1 to value'length) is value;
    variable result : STRING(1 to value'length);
  begin
    if value'length < 1 then
      return NUS;
    else
      for i in ivalue'range loop
        result(i) := MVL9_to_char(iValue(i));
      end loop;
      return result;
    end if;
  end function to_string;

  -------------------------------------------------------------------
  -- TO_HSTRING
  -------------------------------------------------------------------
  function to_hstring (value     : STD_ULOGIC_VECTOR) return STRING is
    constant ne     : INTEGER := (value'length+3)/4;
    variable pad    : STD_ULOGIC_VECTOR(0 to (ne*4 - value'length) - 1);
    variable ivalue : STD_ULOGIC_VECTOR(0 to ne*4 - 1);
    variable result : STRING(1 to ne);
    variable quad   : STD_ULOGIC_VECTOR(0 to 3);
  begin
    if value'length < 1 then
      return NUS;
    else
      if value (value'left) = 'Z' then
        pad := (others => 'Z');
      else
        pad := (others => '0');
      end if;
      ivalue := pad & value;
      for i in 0 to ne-1 loop
        quad := To_X01Z(ivalue(4*i to 4*i+3));
        case quad is
          when x"0"   => result(i+1) := '0';
          when x"1"   => result(i+1) := '1';
          when x"2"   => result(i+1) := '2';
          when x"3"   => result(i+1) := '3';
          when x"4"   => result(i+1) := '4';
          when x"5"   => result(i+1) := '5';
          when x"6"   => result(i+1) := '6';
          when x"7"   => result(i+1) := '7';
          when x"8"   => result(i+1) := '8';
          when x"9"   => result(i+1) := '9';
          when x"A"   => result(i+1) := 'A';
          when x"B"   => result(i+1) := 'B';
          when x"C"   => result(i+1) := 'C';
          when x"D"   => result(i+1) := 'D';
          when x"E"   => result(i+1) := 'E';
          when x"F"   => result(i+1) := 'F';
          when "ZZZZ" => result(i+1) := 'Z';
          when others => result(i+1) := 'X';
        end case;
      end loop;
      return result;
    end if;
  end function to_hstring;

  -------------------------------------------------------------------
  -- TO_OSTRING
  -------------------------------------------------------------------
  function to_ostring (value     : STD_ULOGIC_VECTOR) return STRING is
    constant ne     : INTEGER := (value'length+2)/3;
    variable pad    : STD_ULOGIC_VECTOR(0 to (ne*3 - value'length) - 1);
    variable ivalue : STD_ULOGIC_VECTOR(0 to ne*3 - 1);
    variable result : STRING(1 to ne);
    variable tri    : STD_ULOGIC_VECTOR(0 to 2);
  begin
    if value'length < 1 then
      return NUS;
    else
      if value (value'left) = 'Z' then
        pad := (others => 'Z');
      else
        pad := (others => '0');
      end if;
      ivalue := pad & value;
      for i in 0 to ne-1 loop
        tri := To_X01Z(ivalue(3*i to 3*i+2));
        case tri is
          when o"0"   => result(i+1) := '0';
          when o"1"   => result(i+1) := '1';
          when o"2"   => result(i+1) := '2';
          when o"3"   => result(i+1) := '3';
          when o"4"   => result(i+1) := '4';
          when o"5"   => result(i+1) := '5';
          when o"6"   => result(i+1) := '6';
          when o"7"   => result(i+1) := '7';
          when "ZZZ"  => result(i+1) := 'Z';
          when others => result(i+1) := 'X';
        end case;
      end loop;
      return result;
    end if;
  end function to_ostring;

  function to_string (value     : STD_LOGIC_VECTOR) return STRING is
  begin
    return to_string (to_stdulogicvector (value));
  end function to_string;

  function to_hstring (value     : STD_LOGIC_VECTOR) return STRING is
  begin
    return to_hstring (to_stdulogicvector (value));
  end function to_hstring;

  function to_ostring (value     : STD_LOGIC_VECTOR) return STRING  is
  begin
    return to_ostring (to_stdulogicvector (value));
  end function to_ostring;

  -- rtl_synthesis on
-- pragma synthesis_on
  function maximum (L, R : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR is
  begin  -- function maximum
    if L > R then return L;
    else return R;
    end if;
  end function maximum;

  -- std_logic_vector output
  function minimum (L, R : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR is
  begin  -- function minimum
    if L > R then return R;
    else return L;
    end if;
  end function minimum;

  function maximum (L, R : STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR is
  begin  -- function maximum
    if L > R then return L;
    else return R;
    end if;
  end function maximum;

  -- std_logic_vector output
  function minimum (L, R : STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR is
  begin  -- function minimum
    if L > R then return R;
    else return L;
    end if;
  end function minimum;

  function maximum (L, R : STD_ULOGIC) return STD_ULOGIC is
  begin  -- function maximum
    if L > R then return L;
    else return R;
    end if;
  end function maximum;

  -- std_logic_vector output
  function minimum (L, R : STD_ULOGIC) return STD_ULOGIC is
  begin  -- function minimum
    if L > R then return R;
    else return L;
    end if;
  end function minimum;
end package body std_logic_1164_additions;
