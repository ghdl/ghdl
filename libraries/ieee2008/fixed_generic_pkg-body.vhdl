-- -----------------------------------------------------------------
-- 
-- Copyright 2019 IEEE P1076 WG Authors
-- 
-- See the LICENSE file distributed with this work for copyright and
-- licensing information and the AUTHORS file.
-- 
-- This file to you under the Apache License, Version 2.0 (the "License").
-- You may obtain a copy of the License at
-- 
--     http://www.apache.org/licenses/LICENSE-2.0
-- 
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
-- implied.  See the License for the specific language governing
-- permissions and limitations under the License.
--
--   Title     :  Fixed-point package (Generic package body)
--             :
--   Library   :  This package shall be compiled into a library
--             :  symbolically named IEEE.
--             :
--   Developers:  Accellera VHDL-TC and IEEE P1076 Working Group
--             :
--   Purpose   :  This packages defines basic binary fixed point arithmetic
--             :  arithmetic functions
--             :
--   Note      :  This package may be modified to include additional data
--             :  required by tools, but it must in no way change the
--             :  external interfaces or simulation behavior of the
--             :  description. It is permissible to add comments and/or
--             :  attributes to the package declarations, but not to change
--             :  or delete any original lines of the package declaration.
--             :  The package body may be changed only in accordance with
--             :  the terms of Clause 16 of this standard.
--             :
-- --------------------------------------------------------------------
-- $Revision: 1220 $
-- $Date: 2008-04-10 17:16:09 +0930 (Thu, 10 Apr 2008) $
-- --------------------------------------------------------------------

library IEEE;
use IEEE.MATH_REAL.all;

package body fixed_generic_pkg is
  -- Author David Bishop (dbishop@vhdl.org)
  -- Other contributers: Jim Lewis, Yannick Grugni, Ryan W. Hilton
  -- null array constants
  constant NAUF : UNRESOLVED_ufixed (0 downto 1) := (others => '0');
  constant NASF : UNRESOLVED_sfixed (0 downto 1) := (others => '0');
  constant NSLV : STD_ULOGIC_VECTOR (0 downto 1) := (others => '0');

  -- This differed constant will tell you if the package body is synthesizable
  -- or implemented as real numbers, set to "true" if synthesizable.
  constant fixedsynth_or_real : BOOLEAN := true;

  -- Special version of "minimum" to do some boundary checking without errors
  function mins (l, r : INTEGER)
    return INTEGER is
  begin  -- function mins
    if (l = INTEGER'low or r = INTEGER'low) then
      return 0;                         -- error condition, silent
    end if;
    return minimum (l, r);
  end function mins;

  -- Special version of "minimum" to do some boundary checking with errors
  function mine (l, r : INTEGER)
    return INTEGER is
  begin  -- function mine
    if (l = INTEGER'low or r = INTEGER'low) then
      report fixed_generic_pkg'instance_name
        & " Unbounded number passed, was a literal used?"
        severity error;
      return 0;
    end if;
    return minimum (l, r);
  end function mine;

  -- The following functions are used only internally.  Every function
  -- calls "cleanvec" either directly or indirectly.
  -- purpose: Fixes "downto" problem and resolves meta states
  function cleanvec (
    arg : UNRESOLVED_sfixed)            -- input
    return UNRESOLVED_sfixed
  is
  begin  -- function cleanvec
    assert not (arg'ascending and (arg'low /= INTEGER'low))
      report fixed_generic_pkg'instance_name
      & " Vector passed using a ""to"" range, expected is ""downto"""
      severity error;
    return arg;
  end function cleanvec;

  -- purpose: Fixes "downto" problem and resolves meta states
  function cleanvec (
    arg : UNRESOLVED_ufixed)            -- input
    return UNRESOLVED_ufixed
  is
  begin  -- function cleanvec
    assert not (arg'ascending and (arg'low /= INTEGER'low))
      report fixed_generic_pkg'instance_name
      & " Vector passed using a ""to"" range, expected is ""downto"""
      severity error;
    return arg;
  end function cleanvec;

  -- Type convert a "unsigned" into a "ufixed", used internally
  function to_fixed (
    arg                  : UNRESOLVED_UNSIGNED;  -- shifted vector
    constant left_index  : INTEGER;
    constant right_index : INTEGER)
    return UNRESOLVED_ufixed
  is
    variable result : UNRESOLVED_ufixed (left_index downto right_index);
  begin  -- function to_fixed
    result := UNRESOLVED_ufixed(arg);
    return result;
  end function to_fixed;

  -- Type convert a "signed" into an "sfixed", used internally
  function to_fixed (
    arg                  : UNRESOLVED_SIGNED;  -- shifted vector
    constant left_index  : INTEGER;
    constant right_index : INTEGER)
    return UNRESOLVED_sfixed
  is
    variable result : UNRESOLVED_sfixed (left_index downto right_index);
  begin  -- function to_fixed
    result := UNRESOLVED_sfixed(arg);
    return result;
  end function to_fixed;

  -- Type convert a "ufixed" into an "unsigned", used internally
  function to_uns (
    arg : UNRESOLVED_ufixed)            -- fp vector
    return UNRESOLVED_UNSIGNED
  is
    subtype t is UNRESOLVED_UNSIGNED(arg'high - arg'low downto 0);
    variable slv : t;
  begin  -- function to_uns
    slv := t(arg);
    return slv;
  end function to_uns;

  -- Type convert an "sfixed" into a "signed", used internally
  function to_s (
    arg : UNRESOLVED_sfixed)            -- fp vector
    return UNRESOLVED_SIGNED
  is
    subtype t is UNRESOLVED_SIGNED(arg'high - arg'low downto 0);
    variable slv : t;
  begin  -- function to_s
    slv := t(arg);
    return slv;
  end function to_s;

  -- adds 1 to the LSB of the number
  procedure round_up (arg       : in  UNRESOLVED_ufixed;
                      result    : out UNRESOLVED_ufixed;
                      overflowx : out BOOLEAN) is
    variable arguns, resuns : UNRESOLVED_UNSIGNED (arg'high-arg'low+1 downto 0)
      := (others => '0');
  begin  -- round_up
    arguns (arguns'high-1 downto 0) := to_uns (arg);
    resuns                          := arguns + 1;
    result := to_fixed(resuns(arg'high-arg'low
                              downto 0), arg'high, arg'low);
    overflowx := (resuns(resuns'high) = '1');
  end procedure round_up;

  -- adds 1 to the LSB of the number
  procedure round_up (arg       : in  UNRESOLVED_sfixed;
                      result    : out UNRESOLVED_sfixed;
                      overflowx : out BOOLEAN) is
    variable args, ress : UNRESOLVED_SIGNED (arg'high-arg'low+1 downto 0);
  begin  -- round_up
    args (args'high-1 downto 0) := to_s (arg);
    args(args'high)             := arg(arg'high);  -- sign extend
    ress                        := args + 1;
    result := to_fixed(ress (ress'high-1
                             downto 0), arg'high, arg'low);
    overflowx := ((arg(arg'high) /= ress(ress'high-1))
                  and (or (STD_ULOGIC_VECTOR(ress)) /= '0'));
  end procedure round_up;

  -- Rounding - Performs a "round_nearest" (IEEE 754) which rounds up
  -- when the remainder is > 0.5.  If the remainder IS 0.5 then if the
  -- bottom bit is a "1" it is rounded, otherwise it remains the same.
  function round_fixed (arg            : UNRESOLVED_ufixed;
                        remainder      : UNRESOLVED_ufixed;
                        overflow_style : fixed_overflow_style_type := fixed_overflow_style)
    return UNRESOLVED_ufixed
  is
    variable rounds         : BOOLEAN;
    variable round_overflow : BOOLEAN;
    variable result         : UNRESOLVED_ufixed (arg'range);
  begin
    rounds := false;
    if (remainder'length > 1) then
      if (remainder (remainder'high) = '1') then
        rounds := (arg(arg'low) = '1')
                  or (or (to_sulv(remainder(remainder'high-1 downto
                                           remainder'low))) = '1');
      end if;
    else
      rounds := (arg(arg'low) = '1') and (remainder (remainder'high) = '1');
    end if;
    if rounds then
      round_up(arg       => arg,
               result    => result,
               overflowx => round_overflow);
    else
      result := arg;
    end if;
    if (overflow_style = fixed_saturate) and round_overflow then
      result := saturate (result'high, result'low);
    end if;
    return result;
  end function round_fixed;

  -- Rounding case statement
  function round_fixed (arg            : UNRESOLVED_sfixed;
                        remainder      : UNRESOLVED_sfixed;
                        overflow_style : fixed_overflow_style_type := fixed_overflow_style)
    return UNRESOLVED_sfixed
  is
    variable rounds         : BOOLEAN;
    variable round_overflow : BOOLEAN;
    variable result         : UNRESOLVED_sfixed (arg'range);
  begin
    rounds := false;
    if (remainder'length > 1) then
      if (remainder (remainder'high) = '1') then
        rounds := (arg(arg'low) = '1')
                  or (or (to_sulv(remainder(remainder'high-1 downto
                                           remainder'low))) = '1');
      end if;
    else
      rounds := (arg(arg'low) = '1') and (remainder (remainder'high) = '1');
    end if;
    if rounds then
      round_up(arg       => arg,
               result    => result,
               overflowx => round_overflow);
    else
      result := arg;
    end if;
    if round_overflow then
      if (overflow_style = fixed_saturate) then
        if arg(arg'high) = '0' then
          result := saturate (result'high, result'low);
        else
          result := not saturate (result'high, result'low);
        end if;
        -- Sign bit not fixed when wrapping
      end if;
    end if;
    return result;
  end function round_fixed;

  -- converts an sfixed into a ufixed.  The output is the same length as the
  -- input, because abs("1000") = "1000" = 8.
  function to_ufixed (
    arg : UNRESOLVED_sfixed)
    return UNRESOLVED_ufixed
  is
    constant left_index  : INTEGER := arg'high;
    constant right_index : INTEGER := mine(arg'low, arg'low);
    variable xarg        : UNRESOLVED_sfixed(left_index+1 downto right_index);
    variable result      : UNRESOLVED_ufixed(left_index downto right_index);
  begin
    if arg'length < 1 then
      return NAUF;
    end if;
    xarg   := abs(arg);
    result := UNRESOLVED_ufixed (xarg (left_index downto right_index));
    return result;
  end function to_ufixed;

-----------------------------------------------------------------------------
-- Visible functions
-----------------------------------------------------------------------------
  -- Conversion functions.  These are needed for synthesis where typically
  -- the only input and output type is a std_logic_vector.
  function to_sulv (
    arg : UNRESOLVED_ufixed)            -- fixed point vector
    return STD_ULOGIC_VECTOR
  is
    variable intermediate_result : UNRESOLVED_ufixed(arg'length-1 downto 0);
  begin
    if arg'length < 1 then
      return NSLV;
    end if;
    intermediate_result := arg;
    return STD_ULOGIC_VECTOR (intermediate_result);
  end function to_sulv;

  function to_sulv (
    arg : UNRESOLVED_sfixed)            -- fixed point vector
    return STD_ULOGIC_VECTOR
  is
    variable intermediate_result : UNRESOLVED_sfixed(arg'length-1 downto 0);
  begin
    if arg'length < 1 then
      return NSLV;
    end if;
    intermediate_result := arg;
    return STD_ULOGIC_VECTOR (intermediate_result);
  end function to_sulv;

  function to_slv (
    arg : UNRESOLVED_ufixed)            -- fixed point vector
    return STD_LOGIC_VECTOR is
  begin
    return to_sulv(arg);
  end function to_slv;

  function to_slv (
    arg : UNRESOLVED_sfixed)            -- fixed point vector
    return STD_LOGIC_VECTOR is
  begin
    return to_sulv(arg);
  end function to_slv;

  function to_ufixed (
    arg                  : STD_ULOGIC_VECTOR;  -- shifted vector
    constant left_index  : INTEGER;
    constant right_index : INTEGER)
    return UNRESOLVED_ufixed
  is
    variable result : UNRESOLVED_ufixed (left_index downto right_index);
  begin
    if (arg'length < 1 or right_index > left_index) then
      return NAUF;
    end if;
    if (arg'length /= result'length) then
      report fixed_generic_pkg'instance_name & "TO_UFIXED(SLV) "
        & "Vector lengths do not match.  Input length is "
        & INTEGER'image(arg'length) & " and output will be "
        & INTEGER'image(result'length) & " wide."
        severity error;
      return NAUF;
    else
      result := to_fixed (arg         => UNRESOLVED_UNSIGNED(arg),
                          left_index  => left_index,
                          right_index => right_index);
      return result;
    end if;
  end function to_ufixed;

  function to_sfixed (
    arg                  : STD_ULOGIC_VECTOR;  -- shifted vector
    constant left_index  : INTEGER;
    constant right_index : INTEGER)
    return UNRESOLVED_sfixed
  is
    variable result : UNRESOLVED_sfixed (left_index downto right_index);
  begin
    if (arg'length < 1 or right_index > left_index) then
      return NASF;
    end if;
    if (arg'length /= result'length) then
      report fixed_generic_pkg'instance_name & "TO_SFIXED(SLV) "
        & "Vector lengths do not match.  Input length is "
        & INTEGER'image(arg'length) & " and output will be "
        & INTEGER'image(result'length) & " wide."
        severity error;
      return NASF;
    else
      result := to_fixed (arg         => UNRESOLVED_SIGNED(arg),
                          left_index  => left_index,
                          right_index => right_index);
      return result;
    end if;
  end function to_sfixed;

  -- Two's complement number, Grows the vector by 1 bit.
  -- because "abs (1000.000) = 01000.000" or abs(-16) = 16.
  function "abs" (
    arg : UNRESOLVED_sfixed)            -- fixed point input
    return UNRESOLVED_sfixed
  is
    constant left_index  : INTEGER := arg'high;
    constant right_index : INTEGER := mine(arg'low, arg'low);
    variable ressns      : UNRESOLVED_SIGNED (arg'length downto 0);
    variable result      : UNRESOLVED_sfixed (left_index+1 downto right_index);
  begin
    if (arg'length < 1 or result'length < 1) then
      return NASF;
    end if;
    ressns (arg'length-1 downto 0) := to_s (cleanvec (arg));
    ressns (arg'length)            := ressns (arg'length-1);  -- expand sign bit
    result                         := to_fixed (abs(ressns), left_index+1, right_index);
    return result;
  end function "abs";

  -- also grows the vector by 1 bit.
  function "-" (
    arg : UNRESOLVED_sfixed)            -- fixed point input
    return UNRESOLVED_sfixed
  is
    constant left_index  : INTEGER := arg'high+1;
    constant right_index : INTEGER := mine(arg'low, arg'low);
    variable ressns      : UNRESOLVED_SIGNED (arg'length downto 0);
    variable result      : UNRESOLVED_sfixed (left_index downto right_index);
  begin
    if (arg'length < 1 or result'length < 1) then
      return NASF;
    end if;
    ressns (arg'length-1 downto 0) := to_s (cleanvec(arg));
    ressns (arg'length)            := ressns (arg'length-1);  -- expand sign bit
    result                         := to_fixed (-ressns, left_index, right_index);
    return result;
  end function "-";

  -- Addition
  function "+" (
    l, r : UNRESOLVED_ufixed)    -- ufixed(a downto b) + ufixed(c downto d) =
    return UNRESOLVED_ufixed     -- ufixed(max(a,c)+1 downto min(b,d))
  is
    constant left_index       : INTEGER := maximum(l'high, r'high)+1;
    constant right_index      : INTEGER := mine(l'low, r'low);
    variable lresize, rresize : UNRESOLVED_ufixed (left_index downto right_index);
    variable result           : UNRESOLVED_ufixed (left_index downto right_index);
    variable lslv, rslv : UNRESOLVED_UNSIGNED (left_index-right_index
                                               downto 0);
    variable result_slv : UNRESOLVED_UNSIGNED (left_index-right_index
                                               downto 0);
  begin
    if (l'length < 1 or r'length < 1 or result'length < 1) then
      return NAUF;
    end if;
    lresize    := resize (l, left_index, right_index);
    rresize    := resize (r, left_index, right_index);
    lslv       := to_uns (lresize);
    rslv       := to_uns (rresize);
    result_slv := lslv + rslv;
    result     := to_fixed(result_slv, left_index, right_index);
    return result;
  end function "+";

  function "+" (
    l, r : UNRESOLVED_sfixed)    -- sfixed(a downto b) + sfixed(c downto d) =
    return UNRESOLVED_sfixed     -- sfixed(max(a,c)+1 downto min(b,d))
  is
    constant left_index       : INTEGER := maximum(l'high, r'high)+1;
    constant right_index      : INTEGER := mine(l'low, r'low);
    variable lresize, rresize : UNRESOLVED_sfixed (left_index downto right_index);
    variable result           : UNRESOLVED_sfixed (left_index downto right_index);
    variable lslv, rslv       : UNRESOLVED_SIGNED (left_index-right_index downto 0);
    variable result_slv       : UNRESOLVED_SIGNED (left_index-right_index downto 0);
  begin
    if (l'length < 1 or r'length < 1 or result'length < 1) then
      return NASF;
    end if;
    lresize    := resize (l, left_index, right_index);
    rresize    := resize (r, left_index, right_index);
    lslv       := to_s (lresize);
    rslv       := to_s (rresize);
    result_slv := lslv + rslv;
    result     := to_fixed(result_slv, left_index, right_index);
    return result;
  end function "+";

  -- Subtraction
  function "-" (
    l, r : UNRESOLVED_ufixed)    -- ufixed(a downto b) - ufixed(c downto d) =
    return UNRESOLVED_ufixed     -- ufixed(max(a,c)+1 downto min(b,d))
  is
    constant left_index       : INTEGER := maximum(l'high, r'high)+1;
    constant right_index      : INTEGER := mine(l'low, r'low);
    variable lresize, rresize : UNRESOLVED_ufixed (left_index downto right_index);
    variable result           : UNRESOLVED_ufixed (left_index downto right_index);
    variable lslv, rslv : UNRESOLVED_UNSIGNED (left_index-right_index
                                               downto 0);
    variable result_slv : UNRESOLVED_UNSIGNED (left_index-right_index
                                               downto 0);
  begin
    if (l'length < 1 or r'length < 1 or result'length < 1) then
      return NAUF;
    end if;
    lresize    := resize (l, left_index, right_index);
    rresize    := resize (r, left_index, right_index);
    lslv       := to_uns (lresize);
    rslv       := to_uns (rresize);
    result_slv := lslv - rslv;
    result     := to_fixed(result_slv, left_index, right_index);
    return result;
  end function "-";

  function "-" (
    l, r : UNRESOLVED_sfixed)    -- sfixed(a downto b) - sfixed(c downto d) =
    return UNRESOLVED_sfixed     -- sfixed(max(a,c)+1 downto min(b,d))
  is
    constant left_index       : INTEGER := maximum(l'high, r'high)+1;
    constant right_index      : INTEGER := mine(l'low, r'low);
    variable lresize, rresize : UNRESOLVED_sfixed (left_index downto right_index);
    variable result           : UNRESOLVED_sfixed (left_index downto right_index);
    variable lslv, rslv       : UNRESOLVED_SIGNED (left_index-right_index downto 0);
    variable result_slv       : UNRESOLVED_SIGNED (left_index-right_index downto 0);
  begin
    if (l'length < 1 or r'length < 1 or result'length < 1) then
      return NASF;
    end if;
    lresize    := resize (l, left_index, right_index);
    rresize    := resize (r, left_index, right_index);
    lslv       := to_s (lresize);
    rslv       := to_s (rresize);
    result_slv := lslv - rslv;
    result     := to_fixed(result_slv, left_index, right_index);
    return result;
  end function "-";

  function "*" (
    l, r : UNRESOLVED_ufixed)    -- ufixed(a downto b) * ufixed(c downto d) =
    return UNRESOLVED_ufixed     -- ufixed(a+c+1 downto b+d)
  is
    variable lslv       : UNRESOLVED_UNSIGNED (l'length-1 downto 0);
    variable rslv       : UNRESOLVED_UNSIGNED (r'length-1 downto 0);
    variable result_slv : UNRESOLVED_UNSIGNED (r'length+l'length-1 downto 0);
    variable result     : UNRESOLVED_ufixed (l'high + r'high+1 downto
                                             mine(l'low, l'low) + mine(r'low, r'low));
  begin
    if (l'length < 1 or r'length < 1 or
        result'length /= result_slv'length) then
      return NAUF;
    end if;
    lslv       := to_uns (cleanvec(l));
    rslv       := to_uns (cleanvec(r));
    result_slv := lslv * rslv;
    result     := to_fixed (result_slv, result'high, result'low);
    return result;
  end function "*";

  function "*" (
    l, r : UNRESOLVED_sfixed)    -- sfixed(a downto b) * sfixed(c downto d) =
    return UNRESOLVED_sfixed     -- sfixed(a+c+1 downto b+d)
  is
    variable lslv       : UNRESOLVED_SIGNED (l'length-1 downto 0);
    variable rslv       : UNRESOLVED_SIGNED (r'length-1 downto 0);
    variable result_slv : UNRESOLVED_SIGNED (r'length+l'length-1 downto 0);
    variable result     : UNRESOLVED_sfixed (l'high + r'high+1 downto
                                             mine(l'low, l'low) + mine(r'low, r'low));
  begin
    if (l'length < 1 or r'length < 1 or
        result'length /= result_slv'length) then
      return NASF;
    end if;
    lslv       := to_s (cleanvec(l));
    rslv       := to_s (cleanvec(r));
    result_slv := lslv * rslv;
    result     := to_fixed (result_slv, result'high, result'low);
    return result;
  end function "*";

  function "/" (
    l, r : UNRESOLVED_ufixed)    -- ufixed(a downto b) / ufixed(c downto d) =
    return UNRESOLVED_ufixed is         --  ufixed(a-d downto b-c-1)
  begin
    return divide (l, r);
  end function "/";

  function "/" (
    l, r : UNRESOLVED_sfixed)    -- sfixed(a downto b) / sfixed(c downto d) =
    return UNRESOLVED_sfixed is         -- sfixed(a-d+1 downto b-c)
  begin
    return divide (l, r);
  end function "/";

  -- This version of divide gives the user more control
  -- ufixed(a downto b) / ufixed(c downto d) = ufixed(a-d downto b-c-1)
  function divide (
    l, r                 : UNRESOLVED_ufixed;
    constant round_style : fixed_round_style_type := fixed_round_style;
    constant guard_bits  : NATURAL                := fixed_guard_bits)
    return UNRESOLVED_ufixed
  is
    variable result     : UNRESOLVED_ufixed (l'high - mine(r'low, r'low) downto
                                         mine (l'low, l'low) - r'high -1);
    variable dresult    : UNRESOLVED_ufixed (result'high downto result'low -guard_bits);
    variable lresize    : UNRESOLVED_ufixed (l'high downto l'high - dresult'length+1);
    variable lslv       : UNRESOLVED_UNSIGNED (lresize'length-1 downto 0);
    variable rslv       : UNRESOLVED_UNSIGNED (r'length-1 downto 0);
    variable result_slv : UNRESOLVED_UNSIGNED (lresize'length-1 downto 0);
  begin
    if (l'length < 1 or r'length < 1 or
        mins(r'low, r'low) /= r'low or mins(l'low, l'low) /= l'low) then
      return NAUF;
    end if;
    lresize := resize (arg            => l,
                       left_index     => lresize'high,
                       right_index    => lresize'low,
                       overflow_style => fixed_wrap,   -- vector only grows
                       round_style    => fixed_truncate);
    lslv := to_uns (cleanvec (lresize));
    rslv := to_uns (cleanvec (r));
    if (rslv = 0) then
      report fixed_generic_pkg'instance_name
        & "DIVIDE(ufixed) Division by zero" severity error;
      result := saturate (result'high, result'low);    -- saturate
    else
      result_slv := lslv / rslv;
      dresult    := to_fixed (result_slv, dresult'high, dresult'low);
      result := resize (arg            => dresult,
                        left_index     => result'high,
                        right_index    => result'low,
                        overflow_style => fixed_wrap,  -- overflow impossible
                        round_style    => round_style);
    end if;
    return result;
  end function divide;

  -- sfixed(a downto b) / sfixed(c downto d) = sfixed(a-d+1 downto b-c)
  function divide (
    l, r                 : UNRESOLVED_sfixed;
    constant round_style : fixed_round_style_type := fixed_round_style;
    constant guard_bits  : NATURAL                := fixed_guard_bits)
    return UNRESOLVED_sfixed
  is
    variable result     : UNRESOLVED_sfixed (l'high - mine(r'low, r'low) + 1 downto
                                             mine (l'low, l'low) - r'high);
    variable dresult    : UNRESOLVED_sfixed (result'high downto result'low-guard_bits);
    variable lresize    : UNRESOLVED_sfixed (l'high+1 downto l'high+1 -dresult'length+1);
    variable lslv       : UNRESOLVED_SIGNED (lresize'length-1 downto 0);
    variable rslv       : UNRESOLVED_SIGNED (r'length-1 downto 0);
    variable result_slv : UNRESOLVED_SIGNED (lresize'length-1 downto 0);
  begin
    if (l'length < 1 or r'length < 1 or
        mins(r'low, r'low) /= r'low or mins(l'low, l'low) /= l'low) then
      return NASF;
    end if;
    lresize := resize (arg            => l,
                       left_index     => lresize'high,
                       right_index    => lresize'low,
                       overflow_style => fixed_wrap,   -- vector only grows
                       round_style    => fixed_truncate);
    lslv := to_s (cleanvec (lresize));
    rslv := to_s (cleanvec (r));
    if (rslv = 0) then
      report fixed_generic_pkg'instance_name
        & "DIVIDE(sfixed) Division by zero" severity error;
      result := saturate (result'high, result'low);
    else
      result_slv := lslv / rslv;
      dresult    := to_fixed (result_slv, dresult'high, dresult'low);
      result := resize (arg            => dresult,
                        left_index     => result'high,
                        right_index    => result'low,
                        overflow_style => fixed_wrap,  -- overflow impossible
                        round_style    => round_style);
    end if;
    return result;
  end function divide;

  -- 1 / ufixed(a downto b) = ufixed(-b downto -a-1)
  function reciprocal (
    arg                  : UNRESOLVED_ufixed;  -- fixed point input
    constant round_style : fixed_round_style_type := fixed_round_style;
    constant guard_bits  : NATURAL                := fixed_guard_bits)
    return UNRESOLVED_ufixed
  is
    constant one : UNRESOLVED_ufixed (0 downto 0) := "1";
  begin
    return divide (l           => one,
                   r           => arg,
                   round_style => round_style,
                   guard_bits  => guard_bits);
  end function reciprocal;

  -- 1 / sfixed(a downto b) = sfixed(-b+1 downto -a)
  function reciprocal (
    arg                  : UNRESOLVED_sfixed;              -- fixed point input
    constant round_style : fixed_round_style_type := fixed_round_style;
    constant guard_bits  : NATURAL                := fixed_guard_bits)
    return UNRESOLVED_sfixed
  is
    constant one     : UNRESOLVED_sfixed (1 downto 0) := "01";  -- extra bit.
    variable resultx : UNRESOLVED_sfixed (-mine(arg'low, arg'low)+2 downto -arg'high);
  begin
    if (arg'length < 1 or resultx'length < 1) then
      return NASF;
    else
      resultx := divide (l           => one,
                         r           => arg,
                         round_style => round_style,
                         guard_bits  => guard_bits);
      return resultx (resultx'high-1 downto resultx'low);  -- remove extra bit
    end if;
  end function reciprocal;

  -- ufixed (a downto b) rem ufixed (c downto d)
  --        = ufixed (min(a,c) downto min(b,d))
  function "rem" (
    l, r : UNRESOLVED_ufixed)           -- fixed point input
    return UNRESOLVED_ufixed is
  begin
    return remainder (l, r);
  end function "rem";

  -- remainder
  -- sfixed (a downto b) rem sfixed (c downto d)
  --        = sfixed (min(a,c) downto min(b,d))
  function "rem" (
    l, r : UNRESOLVED_sfixed)           -- fixed point input
    return UNRESOLVED_sfixed is
  begin
    return remainder (l, r);
  end function "rem";

  -- ufixed (a downto b) rem ufixed (c downto d)
  --        = ufixed (min(a,c) downto min(b,d))
  function remainder (
    l, r                 : UNRESOLVED_ufixed;            -- fixed point input
    constant round_style : fixed_round_style_type := fixed_round_style;
    constant guard_bits  : NATURAL                := fixed_guard_bits)
    return UNRESOLVED_ufixed
  is
    variable result     : UNRESOLVED_ufixed (minimum(l'high, r'high) downto
                                             mine(l'low, r'low));
    constant rlow : integer := mins(r'low, r'low);
    variable lresize    : UNRESOLVED_ufixed (maximum(l'high, r'low) downto
                                             rlow-guard_bits);
    variable rresize    : UNRESOLVED_ufixed (r'high downto rlow-guard_bits);
    variable dresult    : UNRESOLVED_ufixed (rresize'range);
    variable lslv       : UNRESOLVED_UNSIGNED (lresize'length-1 downto 0);
    variable rslv       : UNRESOLVED_UNSIGNED (rresize'length-1 downto 0);
    variable result_slv : UNRESOLVED_UNSIGNED (rslv'range);
  begin
    if (l'length < 1 or r'length < 1 or
        mins(r'low, r'low) /= r'low or mins(l'low, l'low) /= l'low) then
      return NAUF;
    end if;
    lresize := resize (arg            => l,
                       left_index     => lresize'high,
                       right_index    => lresize'low,
                       overflow_style => fixed_wrap,     -- vector only grows
                       round_style    => fixed_truncate);
    lslv := to_uns (lresize);
    rresize := resize (arg            => r,
                       left_index     => rresize'high,
                       right_index    => rresize'low,
                       overflow_style => fixed_wrap,     -- vector only grows
                       round_style    => fixed_truncate);
    rslv := to_uns (rresize);
    if (rslv = 0) then
      report fixed_generic_pkg'instance_name
        & "remainder(ufixed) Division by zero" severity error;
      result := saturate (result'high, result'low);      -- saturate
    else
      if (r'low <= l'high) then
        result_slv := lslv rem rslv;
        dresult    := to_fixed (result_slv, dresult'high, dresult'low);
        result := resize (arg            => dresult,
                          left_index     => result'high,
                          right_index    => result'low,
                          overflow_style => fixed_wrap,  -- can't overflow
                          round_style    => round_style);
      end if;
      if l'low < r'low then
        result(mins(r'low-1, l'high) downto l'low) :=
          cleanvec(l(mins(r'low-1, l'high) downto l'low));
      end if;
    end if;
    return result;
  end function remainder;

  -- remainder
  -- sfixed (a downto b) rem sfixed (c downto d)
  --        = sfixed (min(a,c) downto min(b,d))
  function remainder (
    l, r                 : UNRESOLVED_sfixed;  -- fixed point input
    constant round_style : fixed_round_style_type := fixed_round_style;
    constant guard_bits  : NATURAL                := fixed_guard_bits)
    return UNRESOLVED_sfixed
  is
    variable l_abs      : UNRESOLVED_ufixed (l'range);
    variable r_abs      : UNRESOLVED_ufixed (r'range);
    variable result     : UNRESOLVED_sfixed (minimum(r'high, l'high) downto
                                             mine(r'low, l'low));
    variable neg_result : UNRESOLVED_sfixed (minimum(r'high, l'high)+1 downto
                                             mins(r'low, l'low));
  begin
    if (l'length < 1 or r'length < 1 or
        mins(r'low, r'low) /= r'low or mins(l'low, l'low) /= l'low) then
      return NASF;
    end if;
    l_abs := to_ufixed (l);
    r_abs := to_ufixed (r);
    result := UNRESOLVED_sfixed (remainder (
      l           => l_abs,
      r           => r_abs,
      round_style => round_style));
    neg_result := -result;
    if l(l'high) = '1' then
      result := neg_result(result'range);
    end if;
    return result;
  end function remainder;

  -- modulo
  -- ufixed (a downto b) mod ufixed (c downto d)
  --        = ufixed (min(a,c) downto min(b, d))
  function "mod" (
    l, r : UNRESOLVED_ufixed)           -- fixed point input
    return UNRESOLVED_ufixed is
  begin
    return modulo (l, r);
  end function "mod";

  -- sfixed (a downto b) mod sfixed (c downto d)
  --        = sfixed (c downto min(b, d))
  function "mod" (
    l, r : UNRESOLVED_sfixed)           -- fixed point input
    return UNRESOLVED_sfixed is
  begin
    return modulo(l, r);
  end function "mod";

  -- modulo
  -- ufixed (a downto b) mod ufixed (c downto d)
  --        = ufixed (min(a,c) downto min(b, d))
  function modulo (
    l, r                 : UNRESOLVED_ufixed;  -- fixed point input
    constant round_style : fixed_round_style_type := fixed_round_style;
    constant guard_bits  : NATURAL                := fixed_guard_bits)
    return UNRESOLVED_ufixed is
  begin
    return remainder(l           => l,
                     r           => r,
                     round_style => round_style,
                     guard_bits  => guard_bits);
  end function modulo;

  -- sfixed (a downto b) mod sfixed (c downto d)
  --        = sfixed (c downto min(b, d))
  function modulo (
    l, r                    : UNRESOLVED_sfixed;  -- fixed point input
    constant overflow_style : fixed_overflow_style_type := fixed_overflow_style;
    constant round_style    : fixed_round_style_type    := fixed_round_style;
    constant guard_bits     : NATURAL                   := fixed_guard_bits)
    return UNRESOLVED_sfixed
  is
    variable l_abs : UNRESOLVED_ufixed (l'range);
    variable r_abs : UNRESOLVED_ufixed (r'range);
    variable result : UNRESOLVED_sfixed (r'high downto
                                         mine(r'low, l'low));
    variable dresult : UNRESOLVED_sfixed (minimum(r'high, l'high)+1 downto
                                          mins(r'low, l'low));
    variable dresult_not_zero : BOOLEAN;
  begin
    if (l'length < 1 or r'length < 1 or
        mins(r'low, r'low) /= r'low or mins(l'low, l'low) /= l'low) then
      return NASF;
    end if;
    l_abs := to_ufixed (l);
    r_abs := to_ufixed (r);
    dresult := "0" & UNRESOLVED_sfixed(remainder (l           => l_abs,
                                                  r           => r_abs,
                                                  round_style => round_style));
    if (to_s(dresult) = 0) then
      dresult_not_zero := false;
    else
      dresult_not_zero := true;
    end if;
    if to_x01(l(l'high)) = '1' and to_x01(r(r'high)) = '0'
      and dresult_not_zero then
      result := resize (arg            => r - dresult,
                        left_index     => result'high,
                        right_index    => result'low,
                        overflow_style => overflow_style,
                        round_style    => round_style);
    elsif to_x01(l(l'high)) = '1' and to_x01(r(r'high)) = '1' then
      result := resize (arg            => -dresult,
                        left_index     => result'high,
                        right_index    => result'low,
                        overflow_style => overflow_style,
                        round_style    => round_style);
    elsif to_x01(l(l'high)) = '0' and to_x01(r(r'high)) = '1'
      and dresult_not_zero then
      result := resize (arg            => dresult + r,
                        left_index     => result'high,
                        right_index    => result'low,
                        overflow_style => overflow_style,
                        round_style    => round_style);
    else
      result := resize (arg            => dresult,
                        left_index     => result'high,
                        right_index    => result'low,
                        overflow_style => overflow_style,
                        round_style    => round_style);
    end if;
    return result;
  end function modulo;

  -- Procedure for those who need an "accumulator" function
  procedure add_carry (
    L, R   : in  UNRESOLVED_ufixed;
    c_in   : in  STD_ULOGIC;
    result : out UNRESOLVED_ufixed;
    c_out  : out STD_ULOGIC) is
    constant left_index       : INTEGER := maximum(L'high, R'high)+1;
    constant right_index      : INTEGER := mins(L'low, R'low);
    variable lresize, rresize : UNRESOLVED_ufixed (left_index downto right_index);
    variable lslv, rslv : UNRESOLVED_UNSIGNED (left_index-right_index
                                               downto 0);
    variable result_slv : UNRESOLVED_UNSIGNED (left_index-right_index
                                               downto 0);
    variable cx : UNRESOLVED_UNSIGNED (0 downto 0);  -- Carry in
  begin
    if (L'length < 1 or R'length < 1) then
      result := NAUF;
      c_out  := '0';
    else
      cx (0)     := c_in;
      lresize    := resize (L, left_index, right_index);
      rresize    := resize (R, left_index, right_index);
      lslv       := to_uns (lresize);
      rslv       := to_uns (rresize);
      result_slv := lslv + rslv + cx;
      c_out      := result_slv(left_index-right_index);
      result := to_fixed(result_slv (left_index-right_index-1 downto 0),
                         left_index-1, right_index);
    end if;
  end procedure add_carry;

  procedure add_carry (
    L, R   : in  UNRESOLVED_sfixed;
    c_in   : in  STD_ULOGIC;
    result : out UNRESOLVED_sfixed;
    c_out  : out STD_ULOGIC) is
    constant left_index       : INTEGER := maximum(L'high, R'high)+1;
    constant right_index      : INTEGER := mins(L'low, R'low);
    variable lresize, rresize : UNRESOLVED_sfixed (left_index downto right_index);
    variable lslv, rslv : UNRESOLVED_SIGNED (left_index-right_index
                                             downto 0);
    variable result_slv : UNRESOLVED_SIGNED (left_index-right_index
                                             downto 0);
    variable cx : UNRESOLVED_SIGNED (1 downto 0);  -- Carry in
  begin
    if (L'length < 1 or R'length < 1) then
      result := NASF;
      c_out  := '0';
    else
      cx (1)     := '0';
      cx (0)     := c_in;
      lresize    := resize (L, left_index, right_index);
      rresize    := resize (R, left_index, right_index);
      lslv       := to_s (lresize);
      rslv       := to_s (rresize);
      result_slv := lslv + rslv + cx;
      c_out      := result_slv(left_index-right_index);
      result := to_fixed(result_slv (left_index-right_index-1 downto 0),
                         left_index-1, right_index);
    end if;
  end procedure add_carry;

  -- Scales the result by a power of 2.  Width of input = width of output with
  -- the decimal point moved.
  function scalb (y : UNRESOLVED_ufixed; N : INTEGER)
    return UNRESOLVED_ufixed
  is
    variable result : UNRESOLVED_ufixed (y'high+N downto y'low+N);
  begin
    if y'length < 1 then
      return NAUF;
    else
      result := y;
      return result;
    end if;
  end function scalb;

  function scalb (y : UNRESOLVED_ufixed; N : UNRESOLVED_SIGNED)
    return UNRESOLVED_ufixed is
  begin
    return scalb (y => y,
                  N => to_integer(N));
  end function scalb;

  function scalb (y : UNRESOLVED_sfixed; N : INTEGER)
    return UNRESOLVED_sfixed
  is
    variable result : UNRESOLVED_sfixed (y'high+N downto y'low+N);
  begin
    if y'length < 1 then
      return NASF;
    else
      result := y;
      return result;
    end if;
  end function scalb;

  function scalb (y : UNRESOLVED_sfixed; N : UNRESOLVED_SIGNED)
    return UNRESOLVED_sfixed is
  begin
    return scalb (y => y,
                  N => to_integer(N));
  end function scalb;

  function Is_Negative (arg : UNRESOLVED_sfixed) return BOOLEAN is
  begin
    if to_X01(arg(arg'high)) = '1' then
      return true;
    else
      return false;
    end if;
  end function Is_Negative;

  function find_rightmost (arg : UNRESOLVED_ufixed; y : STD_ULOGIC)
    return INTEGER is
  begin
    for_loop : for i in arg'reverse_range loop
      if arg(i) ?= y then
        return i;
      end if;
    end loop;
    return arg'high+1;                  -- return out of bounds 'high
  end function find_rightmost;

  function find_leftmost (arg : UNRESOLVED_ufixed; y : STD_ULOGIC)
    return INTEGER is
  begin
    for_loop : for i in arg'range loop
      if arg(i) ?= y then
        return i;
      end if;
    end loop;
    return arg'low-1;                   -- return out of bounds 'low
  end function find_leftmost;

  function find_rightmost (arg : UNRESOLVED_sfixed; y : STD_ULOGIC)
    return INTEGER is
  begin
    for_loop : for i in arg'reverse_range loop
      if arg(i) ?= y then
        return i;
      end if;
    end loop;
    return arg'high+1;                  -- return out of bounds 'high
  end function find_rightmost;

  function find_leftmost (arg : UNRESOLVED_sfixed; y : STD_ULOGIC)
    return INTEGER is
  begin
    for_loop : for i in arg'range loop
      if arg(i) ?= y then
        return i;
      end if;
    end loop;
    return arg'low-1;                   -- return out of bounds 'low
  end function find_leftmost;

  function "sll" (ARG : UNRESOLVED_ufixed; COUNT : INTEGER)
    return UNRESOLVED_ufixed
  is
    variable argslv : UNRESOLVED_UNSIGNED (ARG'length-1 downto 0);
    variable result : UNRESOLVED_ufixed (ARG'range);
  begin
    argslv := to_uns (ARG);
    argslv := argslv sll COUNT;
    result := to_fixed (argslv, result'high, result'low);
    return result;
  end function "sll";

  function "srl" (ARG : UNRESOLVED_ufixed; COUNT : INTEGER)
    return UNRESOLVED_ufixed
  is
    variable argslv : UNRESOLVED_UNSIGNED (ARG'length-1 downto 0);
    variable result : UNRESOLVED_ufixed (ARG'range);
  begin
    argslv := to_uns (ARG);
    argslv := argslv srl COUNT;
    result := to_fixed (argslv, result'high, result'low);
    return result;
  end function "srl";

  function "rol" (ARG : UNRESOLVED_ufixed; COUNT : INTEGER)
    return UNRESOLVED_ufixed
  is
    variable argslv : UNRESOLVED_UNSIGNED (ARG'length-1 downto 0);
    variable result : UNRESOLVED_ufixed (ARG'range);
  begin
    argslv := to_uns (ARG);
    argslv := argslv rol COUNT;
    result := to_fixed (argslv, result'high, result'low);
    return result;
  end function "rol";

  function "ror" (ARG : UNRESOLVED_ufixed; COUNT : INTEGER)
    return UNRESOLVED_ufixed
  is
    variable argslv : UNRESOLVED_UNSIGNED (ARG'length-1 downto 0);
    variable result : UNRESOLVED_ufixed (ARG'range);
  begin
    argslv := to_uns (ARG);
    argslv := argslv ror COUNT;
    result := to_fixed (argslv, result'high, result'low);
    return result;
  end function "ror";

  function "sla" (ARG : UNRESOLVED_ufixed; COUNT : INTEGER)
    return UNRESOLVED_ufixed
  is
    variable argslv : UNRESOLVED_UNSIGNED (ARG'length-1 downto 0);
    variable result : UNRESOLVED_ufixed (ARG'range);
  begin
    argslv := to_uns (ARG);
    -- Arithmetic shift on an unsigned is a logical shift
    argslv := argslv sll COUNT;
    result := to_fixed (argslv, result'high, result'low);
    return result;
  end function "sla";

  function "sra" (ARG : UNRESOLVED_ufixed; COUNT : INTEGER)
    return UNRESOLVED_ufixed
  is
    variable argslv : UNRESOLVED_UNSIGNED (ARG'length-1 downto 0);
    variable result : UNRESOLVED_ufixed (ARG'range);
  begin
    argslv := to_uns (ARG);
    -- Arithmetic shift on an unsigned is a logical shift
    argslv := argslv srl COUNT;
    result := to_fixed (argslv, result'high, result'low);
    return result;
  end function "sra";

  function "sll" (ARG : UNRESOLVED_sfixed; COUNT : INTEGER)
    return UNRESOLVED_sfixed
  is
    variable argslv : UNRESOLVED_SIGNED (ARG'length-1 downto 0);
    variable result : UNRESOLVED_sfixed (ARG'range);
  begin
    argslv := to_s (ARG);
    argslv := argslv sll COUNT;
    result := to_fixed (argslv, result'high, result'low);
    return result;
  end function "sll";

  function "srl" (ARG : UNRESOLVED_sfixed; COUNT : INTEGER)
    return UNRESOLVED_sfixed
  is
    variable argslv : UNRESOLVED_SIGNED (ARG'length-1 downto 0);
    variable result : UNRESOLVED_sfixed (ARG'range);
  begin
    argslv := to_s (ARG);
    argslv := argslv srl COUNT;
    result := to_fixed (argslv, result'high, result'low);
    return result;
  end function "srl";

  function "rol" (ARG : UNRESOLVED_sfixed; COUNT : INTEGER)
    return UNRESOLVED_sfixed
  is
    variable argslv : UNRESOLVED_SIGNED (ARG'length-1 downto 0);
    variable result : UNRESOLVED_sfixed (ARG'range);
  begin
    argslv := to_s (ARG);
    argslv := argslv rol COUNT;
    result := to_fixed (argslv, result'high, result'low);
    return result;
  end function "rol";

  function "ror" (ARG : UNRESOLVED_sfixed; COUNT : INTEGER)
    return UNRESOLVED_sfixed
  is
    variable argslv : UNRESOLVED_SIGNED (ARG'length-1 downto 0);
    variable result : UNRESOLVED_sfixed (ARG'range);
  begin
    argslv := to_s (ARG);
    argslv := argslv ror COUNT;
    result := to_fixed (argslv, result'high, result'low);
    return result;
  end function "ror";

  function "sla" (ARG : UNRESOLVED_sfixed; COUNT : INTEGER)
    return UNRESOLVED_sfixed
  is
    variable argslv : UNRESOLVED_SIGNED (ARG'length-1 downto 0);
    variable result : UNRESOLVED_sfixed (ARG'range);
  begin
    argslv := to_s (ARG);
    if COUNT > 0 then
      -- Arithmetic shift left on a 2's complement number is a logic shift
      argslv := argslv sll COUNT;
    else
      argslv := argslv sra -COUNT;
    end if;
    result := to_fixed (argslv, result'high, result'low);
    return result;
  end function "sla";

  function "sra" (ARG : UNRESOLVED_sfixed; COUNT : INTEGER)
    return UNRESOLVED_sfixed
  is
    variable argslv : UNRESOLVED_SIGNED (ARG'length-1 downto 0);
    variable result : UNRESOLVED_sfixed (ARG'range);
  begin
    argslv := to_s (ARG);
    if COUNT > 0 then
      argslv := argslv sra COUNT;
    else
      -- Arithmetic shift left on a 2's complement number is a logic shift
      argslv := argslv sll -COUNT;
    end if;
    result := to_fixed (argslv, result'high, result'low);
    return result;
  end function "sra";

  -- Because some people want the older functions.
  function SHIFT_LEFT (ARG : UNRESOLVED_ufixed; COUNT : NATURAL)
    return UNRESOLVED_ufixed is
  begin
    if (ARG'length < 1) then
      return NAUF;
    end if;
    return ARG sla COUNT;
  end function SHIFT_LEFT;

  function SHIFT_RIGHT (ARG : UNRESOLVED_ufixed; COUNT : NATURAL)
    return UNRESOLVED_ufixed is
  begin
    if (ARG'length < 1) then
      return NAUF;
    end if;
    return ARG sra COUNT;
  end function SHIFT_RIGHT;

  function SHIFT_LEFT (ARG : UNRESOLVED_sfixed; COUNT : NATURAL)
    return UNRESOLVED_sfixed is
  begin
    if (ARG'length < 1) then
      return NASF;
    end if;
    return ARG sla COUNT;
  end function SHIFT_LEFT;

  function SHIFT_RIGHT (ARG : UNRESOLVED_sfixed; COUNT : NATURAL)
    return UNRESOLVED_sfixed is
  begin
    if (ARG'length < 1) then
      return NASF;
    end if;
    return ARG sra COUNT;
  end function SHIFT_RIGHT;

  ----------------------------------------------------------------------------
  -- logical functions
  ----------------------------------------------------------------------------
  function "not" (L : UNRESOLVED_ufixed) return UNRESOLVED_ufixed is
    variable RESULT : STD_ULOGIC_VECTOR(L'length-1 downto 0);  -- force downto
  begin
    RESULT := not to_sulv(L);
    return to_ufixed(RESULT, L'high, L'low);
  end function "not";

  function "and" (L, R : UNRESOLVED_ufixed) return UNRESOLVED_ufixed is
    variable RESULT : STD_ULOGIC_VECTOR(L'length-1 downto 0);  -- force downto
  begin
    if (L'high = R'high and L'low = R'low) then
      RESULT := to_sulv(L) and to_sulv(R);
    else
      assert no_warning
        report fixed_generic_pkg'instance_name
        & """and"": Range error L'RANGE /= R'RANGE"
        severity warning;
      RESULT := (others => 'X');
    end if;
    return to_ufixed(RESULT, L'high, L'low);
  end function "and";

  function "or" (L, R : UNRESOLVED_ufixed) return UNRESOLVED_ufixed is
    variable RESULT : STD_ULOGIC_VECTOR(L'length-1 downto 0);  -- force downto
  begin
    if (L'high = R'high and L'low = R'low) then
      RESULT := to_sulv(L) or to_sulv(R);
    else
      assert no_warning
        report fixed_generic_pkg'instance_name
        & """or"": Range error L'RANGE /= R'RANGE"
        severity warning;
      RESULT := (others => 'X');
    end if;
    return to_ufixed(RESULT, L'high, L'low);
  end function "or";

  function "nand" (L, R : UNRESOLVED_ufixed) return UNRESOLVED_ufixed is
    variable RESULT : STD_ULOGIC_VECTOR(L'length-1 downto 0);  -- force downto
  begin
    if (L'high = R'high and L'low = R'low) then
      RESULT := to_sulv(L) nand to_sulv(R);
    else
      assert no_warning
        report fixed_generic_pkg'instance_name
        & """nand"": Range error L'RANGE /= R'RANGE"
        severity warning;
      RESULT := (others => 'X');
    end if;
    return to_ufixed(RESULT, L'high, L'low);
  end function "nand";

  function "nor" (L, R : UNRESOLVED_ufixed) return UNRESOLVED_ufixed is
    variable RESULT : STD_ULOGIC_VECTOR(L'length-1 downto 0);  -- force downto
  begin
    if (L'high = R'high and L'low = R'low) then
      RESULT := to_sulv(L) nor to_sulv(R);
    else
      assert no_warning
        report fixed_generic_pkg'instance_name
        & """nor"": Range error L'RANGE /= R'RANGE"
        severity warning;
      RESULT := (others => 'X');
    end if;
    return to_ufixed(RESULT, L'high, L'low);
  end function "nor";

  function "xor" (L, R : UNRESOLVED_ufixed) return UNRESOLVED_ufixed is
    variable RESULT : STD_ULOGIC_VECTOR(L'length-1 downto 0);  -- force downto
  begin
    if (L'high = R'high and L'low = R'low) then
      RESULT := to_sulv(L) xor to_sulv(R);
    else
      assert no_warning
        report fixed_generic_pkg'instance_name
        & """xor"": Range error L'RANGE /= R'RANGE"
        severity warning;
      RESULT := (others => 'X');
    end if;
    return to_ufixed(RESULT, L'high, L'low);
  end function "xor";

  function "xnor" (L, R : UNRESOLVED_ufixed) return UNRESOLVED_ufixed is
    variable RESULT : STD_ULOGIC_VECTOR(L'length-1 downto 0);  -- force downto
  begin
    if (L'high = R'high and L'low = R'low) then
      RESULT := to_sulv(L) xnor to_sulv(R);
    else
      assert no_warning
        report fixed_generic_pkg'instance_name
        & """xnor"": Range error L'RANGE /= R'RANGE"
        severity warning;
      RESULT := (others => 'X');
    end if;
    return to_ufixed(RESULT, L'high, L'low);
  end function "xnor";

  function "not" (L : UNRESOLVED_sfixed) return UNRESOLVED_sfixed is
    variable RESULT : STD_ULOGIC_VECTOR(L'length-1 downto 0);  -- force downto
  begin
    RESULT := not to_sulv(L);
    return to_sfixed(RESULT, L'high, L'low);
  end function "not";

  function "and" (L, R : UNRESOLVED_sfixed) return UNRESOLVED_sfixed is
    variable RESULT : STD_ULOGIC_VECTOR(L'length-1 downto 0);  -- force downto
  begin
    if (L'high = R'high and L'low = R'low) then
      RESULT := to_sulv(L) and to_sulv(R);
    else
      assert no_warning
        report fixed_generic_pkg'instance_name
        & """and"": Range error L'RANGE /= R'RANGE"
        severity warning;
      RESULT := (others => 'X');
    end if;
    return to_sfixed(RESULT, L'high, L'low);
  end function "and";

  function "or" (L, R : UNRESOLVED_sfixed) return UNRESOLVED_sfixed is
    variable RESULT : STD_ULOGIC_VECTOR(L'length-1 downto 0);  -- force downto
  begin
    if (L'high = R'high and L'low = R'low) then
      RESULT := to_sulv(L) or to_sulv(R);
    else
      assert no_warning
        report fixed_generic_pkg'instance_name
        & """or"": Range error L'RANGE /= R'RANGE"
        severity warning;
      RESULT := (others => 'X');
    end if;
    return to_sfixed(RESULT, L'high, L'low);
  end function "or";

  function "nand" (L, R : UNRESOLVED_sfixed) return UNRESOLVED_sfixed is
    variable RESULT : STD_ULOGIC_VECTOR(L'length-1 downto 0);  -- force downto
  begin
    if (L'high = R'high and L'low = R'low) then
      RESULT := to_sulv(L) nand to_sulv(R);
    else
      assert no_warning
        report fixed_generic_pkg'instance_name
        & """nand"": Range error L'RANGE /= R'RANGE"
        severity warning;
      RESULT := (others => 'X');
    end if;
    return to_sfixed(RESULT, L'high, L'low);
  end function "nand";

  function "nor" (L, R : UNRESOLVED_sfixed) return UNRESOLVED_sfixed is
    variable RESULT : STD_ULOGIC_VECTOR(L'length-1 downto 0);  -- force downto
  begin
    if (L'high = R'high and L'low = R'low) then
      RESULT := to_sulv(L) nor to_sulv(R);
    else
      assert no_warning
        report fixed_generic_pkg'instance_name
        & """nor"": Range error L'RANGE /= R'RANGE"
        severity warning;
      RESULT := (others => 'X');
    end if;
    return to_sfixed(RESULT, L'high, L'low);
  end function "nor";

  function "xor" (L, R : UNRESOLVED_sfixed) return UNRESOLVED_sfixed is
    variable RESULT : STD_ULOGIC_VECTOR(L'length-1 downto 0);  -- force downto
  begin
    if (L'high = R'high and L'low = R'low) then
      RESULT := to_sulv(L) xor to_sulv(R);
    else
      assert no_warning
        report fixed_generic_pkg'instance_name
        & """xor"": Range error L'RANGE /= R'RANGE"
        severity warning;
      RESULT := (others => 'X');
    end if;
    return to_sfixed(RESULT, L'high, L'low);
  end function "xor";

  function "xnor" (L, R : UNRESOLVED_sfixed) return UNRESOLVED_sfixed is
    variable RESULT : STD_ULOGIC_VECTOR(L'length-1 downto 0);  -- force downto
  begin
    if (L'high = R'high and L'low = R'low) then
      RESULT := to_sulv(L) xnor to_sulv(R);
    else
      assert no_warning
        report fixed_generic_pkg'instance_name
        & """xnor"": Range error L'RANGE /= R'RANGE"
        severity warning;
      RESULT := (others => 'X');
    end if;
    return to_sfixed(RESULT, L'high, L'low);
  end function "xnor";

  -- Vector and std_ulogic functions, same as functions in numeric_std
  function "and" (L : STD_ULOGIC; R : UNRESOLVED_ufixed)
    return UNRESOLVED_ufixed
  is
    variable result : UNRESOLVED_ufixed (R'range);
  begin
    for i in result'range loop
      result(i) := L and R(i);
    end loop;
    return result;
  end function "and";

  function "and" (L : UNRESOLVED_ufixed; R : STD_ULOGIC)
    return UNRESOLVED_ufixed
  is
    variable result : UNRESOLVED_ufixed (L'range);
  begin
    for i in result'range loop
      result(i) := L(i) and R;
    end loop;
    return result;
  end function "and";

  function "or" (L : STD_ULOGIC; R : UNRESOLVED_ufixed)
    return UNRESOLVED_ufixed
  is
    variable result : UNRESOLVED_ufixed (R'range);
  begin
    for i in result'range loop
      result(i) := L or R(i);
    end loop;
    return result;
  end function "or";

  function "or" (L : UNRESOLVED_ufixed; R : STD_ULOGIC)
    return UNRESOLVED_ufixed
  is
    variable result : UNRESOLVED_ufixed (L'range);
  begin
    for i in result'range loop
      result(i) := L(i) or R;
    end loop;
    return result;
  end function "or";

  function "nand" (L : STD_ULOGIC; R : UNRESOLVED_ufixed)
    return UNRESOLVED_ufixed
  is
    variable result : UNRESOLVED_ufixed (R'range);
  begin
    for i in result'range loop
      result(i) := L nand R(i);
    end loop;
    return result;
  end function "nand";

  function "nand" (L : UNRESOLVED_ufixed; R : STD_ULOGIC)
    return UNRESOLVED_ufixed
  is
    variable result : UNRESOLVED_ufixed (L'range);
  begin
    for i in result'range loop
      result(i) := L(i) nand R;
    end loop;
    return result;
  end function "nand";

  function "nor" (L : STD_ULOGIC; R : UNRESOLVED_ufixed)
    return UNRESOLVED_ufixed
  is
    variable result : UNRESOLVED_ufixed (R'range);
  begin
    for i in result'range loop
      result(i) := L nor R(i);
    end loop;
    return result;
  end function "nor";

  function "nor" (L : UNRESOLVED_ufixed; R : STD_ULOGIC)
    return UNRESOLVED_ufixed
  is
    variable result : UNRESOLVED_ufixed (L'range);
  begin
    for i in result'range loop
      result(i) := L(i) nor R;
    end loop;
    return result;
  end function "nor";

  function "xor" (L : STD_ULOGIC; R : UNRESOLVED_ufixed)
    return UNRESOLVED_ufixed
  is
    variable result : UNRESOLVED_ufixed (R'range);
  begin
    for i in result'range loop
      result(i) := L xor R(i);
    end loop;
    return result;
  end function "xor";

  function "xor" (L : UNRESOLVED_ufixed; R : STD_ULOGIC)
    return UNRESOLVED_ufixed
  is
    variable result : UNRESOLVED_ufixed (L'range);
  begin
    for i in result'range loop
      result(i) := L(i) xor R;
    end loop;
    return result;
  end function "xor";

  function "xnor" (L : STD_ULOGIC; R : UNRESOLVED_ufixed)
    return UNRESOLVED_ufixed
  is
    variable result : UNRESOLVED_ufixed (R'range);
  begin
    for i in result'range loop
      result(i) := L xnor R(i);
    end loop;
    return result;
  end function "xnor";

  function "xnor" (L : UNRESOLVED_ufixed; R : STD_ULOGIC)
    return UNRESOLVED_ufixed
  is
    variable result : UNRESOLVED_ufixed (L'range);
  begin
    for i in result'range loop
      result(i) := L(i) xnor R;
    end loop;
    return result;
  end function "xnor";

  function "and" (L : STD_ULOGIC; R : UNRESOLVED_sfixed)
    return UNRESOLVED_sfixed
  is
    variable result : UNRESOLVED_sfixed (R'range);
  begin
    for i in result'range loop
      result(i) := L and R(i);
    end loop;
    return result;
  end function "and";

  function "and" (L : UNRESOLVED_sfixed; R : STD_ULOGIC)
    return UNRESOLVED_sfixed
  is
    variable result : UNRESOLVED_sfixed (L'range);
  begin
    for i in result'range loop
      result(i) := L(i) and R;
    end loop;
    return result;
  end function "and";

  function "or" (L : STD_ULOGIC; R : UNRESOLVED_sfixed)
    return UNRESOLVED_sfixed
  is
    variable result : UNRESOLVED_sfixed (R'range);
  begin
    for i in result'range loop
      result(i) := L or R(i);
    end loop;
    return result;
  end function "or";

  function "or" (L : UNRESOLVED_sfixed; R : STD_ULOGIC)
    return UNRESOLVED_sfixed
  is
    variable result : UNRESOLVED_sfixed (L'range);
  begin
    for i in result'range loop
      result(i) := L(i) or R;
    end loop;
    return result;
  end function "or";

  function "nand" (L : STD_ULOGIC; R : UNRESOLVED_sfixed)
    return UNRESOLVED_sfixed
  is
    variable result : UNRESOLVED_sfixed (R'range);
  begin
    for i in result'range loop
      result(i) := L nand R(i);
    end loop;
    return result;
  end function "nand";

  function "nand" (L : UNRESOLVED_sfixed; R : STD_ULOGIC)
    return UNRESOLVED_sfixed
  is
    variable result : UNRESOLVED_sfixed (L'range);
  begin
    for i in result'range loop
      result(i) := L(i) nand R;
    end loop;
    return result;
  end function "nand";

  function "nor" (L : STD_ULOGIC; R : UNRESOLVED_sfixed)
    return UNRESOLVED_sfixed
  is
    variable result : UNRESOLVED_sfixed (R'range);
  begin
    for i in result'range loop
      result(i) := L nor R(i);
    end loop;
    return result;
  end function "nor";

  function "nor" (L : UNRESOLVED_sfixed; R : STD_ULOGIC)
    return UNRESOLVED_sfixed
  is
    variable result : UNRESOLVED_sfixed (L'range);
  begin
    for i in result'range loop
      result(i) := L(i) nor R;
    end loop;
    return result;
  end function "nor";

  function "xor" (L : STD_ULOGIC; R : UNRESOLVED_sfixed)
    return UNRESOLVED_sfixed
  is
    variable result : UNRESOLVED_sfixed (R'range);
  begin
    for i in result'range loop
      result(i) := L xor R(i);
    end loop;
    return result;
  end function "xor";

  function "xor" (L : UNRESOLVED_sfixed; R : STD_ULOGIC)
    return UNRESOLVED_sfixed
  is
    variable result : UNRESOLVED_sfixed (L'range);
  begin
    for i in result'range loop
      result(i) := L(i) xor R;
    end loop;
    return result;
  end function "xor";

  function "xnor" (L : STD_ULOGIC; R : UNRESOLVED_sfixed)
    return UNRESOLVED_sfixed
  is
    variable result : UNRESOLVED_sfixed (R'range);
  begin
    for i in result'range loop
      result(i) := L xnor R(i);
    end loop;
    return result;
  end function "xnor";

  function "xnor" (L : UNRESOLVED_sfixed; R : STD_ULOGIC)
    return UNRESOLVED_sfixed
  is
    variable result : UNRESOLVED_sfixed (L'range);
  begin
    for i in result'range loop
      result(i) := L(i) xnor R;
    end loop;
    return result;
  end function "xnor";

  -- Reduction operators
  function "and" (l : UNRESOLVED_ufixed) return STD_ULOGIC is
  begin
    return and to_sulv(l);
  end function "and";

  function "nand" (l : UNRESOLVED_ufixed) return STD_ULOGIC is
  begin
    return nand to_sulv(l);
  end function "nand";

  function "or" (l : UNRESOLVED_ufixed) return STD_ULOGIC is
  begin
    return or to_sulv(l);
  end function "or";

  function "nor" (l : UNRESOLVED_ufixed) return STD_ULOGIC is
  begin
    return nor to_sulv(l);
  end function "nor";

  function "xor" (l : UNRESOLVED_ufixed) return STD_ULOGIC is
  begin
    return xor to_sulv(l);
  end function "xor";

  function "xnor" (l : UNRESOLVED_ufixed) return STD_ULOGIC is
  begin
    return xnor to_sulv(l);
  end function "xnor";

  function "and" (l : UNRESOLVED_sfixed) return STD_ULOGIC is
  begin
    return and to_sulv(l);
  end function "and";

  function "nand" (l : UNRESOLVED_sfixed) return STD_ULOGIC is
  begin
    return nand to_sulv(l);
  end function "nand";

  function "or" (l : UNRESOLVED_sfixed) return STD_ULOGIC is
  begin
    return or to_sulv(l);
  end function "or";

  function "nor" (l : UNRESOLVED_sfixed) return STD_ULOGIC is
  begin
    return nor to_sulv(l);
  end function "nor";

  function "xor" (l : UNRESOLVED_sfixed) return STD_ULOGIC is
  begin
    return xor to_sulv(l);
  end function "xor";

  function "xnor" (l : UNRESOLVED_sfixed) return STD_ULOGIC is
  begin
    return xnor to_sulv(l);
  end function "xnor";
  -- End reduction operators

  function "?=" (L, R : UNRESOLVED_ufixed) return STD_ULOGIC is
    constant left_index       : INTEGER := maximum(L'high, R'high);
    constant right_index      : INTEGER := mins(L'low, R'low);
    variable lresize, rresize : UNRESOLVED_ufixed (left_index downto right_index);
    variable lslv, rslv       : UNRESOLVED_UNSIGNED (lresize'length-1 downto 0);
  begin  -- ?=
    if ((L'length < 1) or (R'length < 1)) then
      assert no_warning
        report fixed_generic_pkg'instance_name
        & """?="": null detected, returning X"
        severity warning;
      return 'X';
    else
      lresize := resize (L, left_index, right_index);
      rresize := resize (R, left_index, right_index);
      lslv    := to_uns (lresize);
      rslv    := to_uns (rresize);
      return lslv ?= rslv;
    end if;
  end function "?=";

  function "?/=" (L, R : UNRESOLVED_ufixed) return STD_ULOGIC is
    constant left_index       : INTEGER := maximum(L'high, R'high);
    constant right_index      : INTEGER := mins(L'low, R'low);
    variable lresize, rresize : UNRESOLVED_ufixed (left_index downto right_index);
    variable lslv, rslv       : UNRESOLVED_UNSIGNED (lresize'length-1 downto 0);
  begin  -- ?/=
    if ((L'length < 1) or (R'length < 1)) then
      assert no_warning
        report fixed_generic_pkg'instance_name
        & """?/="": null detected, returning X"
        severity warning;
      return 'X';
    else
      lresize := resize (L, left_index, right_index);
      rresize := resize (R, left_index, right_index);
      lslv    := to_uns (lresize);
      rslv    := to_uns (rresize);
      return lslv ?/= rslv;
    end if;
  end function "?/=";

  function "?>" (L, R : UNRESOLVED_ufixed) return STD_ULOGIC is
    constant left_index       : INTEGER := maximum(L'high, R'high);
    constant right_index      : INTEGER := mins(L'low, R'low);
    variable lresize, rresize : UNRESOLVED_ufixed (left_index downto right_index);
    variable lslv, rslv       : UNRESOLVED_UNSIGNED (lresize'length-1 downto 0);
  begin  -- ?>
    if ((L'length < 1) or (R'length < 1)) then
      assert no_warning
        report fixed_generic_pkg'instance_name
        & """?>"": null detected, returning X"
        severity warning;
      return 'X';
    else
      lresize := resize (L, left_index, right_index);
      rresize := resize (R, left_index, right_index);
      lslv    := to_uns (lresize);
      rslv    := to_uns (rresize);
      return lslv ?> rslv;
    end if;
  end function "?>";

  function "?>=" (L, R : UNRESOLVED_ufixed) return STD_ULOGIC is
    constant left_index       : INTEGER := maximum(L'high, R'high);
    constant right_index      : INTEGER := mins(L'low, R'low);
    variable lresize, rresize : UNRESOLVED_ufixed (left_index downto right_index);
    variable lslv, rslv       : UNRESOLVED_UNSIGNED (lresize'length-1 downto 0);
  begin  -- ?>=
    if ((L'length < 1) or (R'length < 1)) then
      assert no_warning
        report fixed_generic_pkg'instance_name
        & """?>="": null detected, returning X"
        severity warning;
      return 'X';
    else
      lresize := resize (L, left_index, right_index);
      rresize := resize (R, left_index, right_index);
      lslv    := to_uns (lresize);
      rslv    := to_uns (rresize);
      return lslv ?>= rslv;
    end if;
  end function "?>=";

  function "?<" (L, R : UNRESOLVED_ufixed) return STD_ULOGIC is
    constant left_index       : INTEGER := maximum(L'high, R'high);
    constant right_index      : INTEGER := mins(L'low, R'low);
    variable lresize, rresize : UNRESOLVED_ufixed (left_index downto right_index);
    variable lslv, rslv       : UNRESOLVED_UNSIGNED (lresize'length-1 downto 0);
  begin  -- ?<
    if ((L'length < 1) or (R'length < 1)) then
      assert no_warning
        report fixed_generic_pkg'instance_name
        & """?<"": null detected, returning X"
        severity warning;
      return 'X';
    else
      lresize := resize (L, left_index, right_index);
      rresize := resize (R, left_index, right_index);
      lslv    := to_uns (lresize);
      rslv    := to_uns (rresize);
      return lslv ?< rslv;
    end if;
  end function "?<";

  function "?<=" (L, R : UNRESOLVED_ufixed) return STD_ULOGIC is
    constant left_index       : INTEGER := maximum(L'high, R'high);
    constant right_index      : INTEGER := mins(L'low, R'low);
    variable lresize, rresize : UNRESOLVED_ufixed (left_index downto right_index);
    variable lslv, rslv       : UNRESOLVED_UNSIGNED (lresize'length-1 downto 0);
  begin  -- ?<=
    if ((L'length < 1) or (R'length < 1)) then
      assert no_warning
        report fixed_generic_pkg'instance_name
        & """?<="": null detected, returning X"
        severity warning;
      return 'X';
    else
      lresize := resize (L, left_index, right_index);
      rresize := resize (R, left_index, right_index);
      lslv    := to_uns (lresize);
      rslv    := to_uns (rresize);
      return lslv ?<= rslv;
    end if;
  end function "?<=";

  function "?=" (L, R : UNRESOLVED_sfixed) return STD_ULOGIC is
    constant left_index       : INTEGER := maximum(L'high, R'high);
    constant right_index      : INTEGER := mins(L'low, R'low);
    variable lresize, rresize : UNRESOLVED_sfixed (left_index downto right_index);
    variable lslv, rslv       : UNRESOLVED_SIGNED (lresize'length-1 downto 0);
  begin  -- ?=
    if ((L'length < 1) or (R'length < 1)) then
      assert no_warning
        report fixed_generic_pkg'instance_name
        & """?="": null detected, returning X"
        severity warning;
      return 'X';
    else
      lresize := resize (L, left_index, right_index);
      rresize := resize (R, left_index, right_index);
      lslv    := to_s (lresize);
      rslv    := to_s (rresize);
      return lslv ?= rslv;
    end if;
  end function "?=";

  function "?/=" (L, R : UNRESOLVED_sfixed) return STD_ULOGIC is
    constant left_index       : INTEGER := maximum(L'high, R'high);
    constant right_index      : INTEGER := mins(L'low, R'low);
    variable lresize, rresize : UNRESOLVED_sfixed (left_index downto right_index);
    variable lslv, rslv       : UNRESOLVED_SIGNED (lresize'length-1 downto 0);
  begin  -- ?/=
    if ((L'length < 1) or (R'length < 1)) then
      assert no_warning
        report fixed_generic_pkg'instance_name
        & """?/="": null detected, returning X"
        severity warning;
      return 'X';
    else
      lresize := resize (L, left_index, right_index);
      rresize := resize (R, left_index, right_index);
      lslv    := to_s (lresize);
      rslv    := to_s (rresize);
      return lslv ?/= rslv;
    end if;
  end function "?/=";

  function "?>" (L, R : UNRESOLVED_sfixed) return STD_ULOGIC is
    constant left_index       : INTEGER := maximum(L'high, R'high);
    constant right_index      : INTEGER := mins(L'low, R'low);
    variable lresize, rresize : UNRESOLVED_sfixed (left_index downto right_index);
    variable lslv, rslv       : UNRESOLVED_SIGNED (lresize'length-1 downto 0);
  begin  -- ?>
    if ((L'length < 1) or (R'length < 1)) then
      assert no_warning
        report fixed_generic_pkg'instance_name
        & """?>"": null detected, returning X"
        severity warning;
      return 'X';
    else
      lresize := resize (L, left_index, right_index);
      rresize := resize (R, left_index, right_index);
      lslv    := to_s (lresize);
      rslv    := to_s (rresize);
      return lslv ?> rslv;
    end if;
  end function "?>";

  function "?>=" (L, R : UNRESOLVED_sfixed) return STD_ULOGIC is
    constant left_index       : INTEGER := maximum(L'high, R'high);
    constant right_index      : INTEGER := mins(L'low, R'low);
    variable lresize, rresize : UNRESOLVED_sfixed (left_index downto right_index);
    variable lslv, rslv       : UNRESOLVED_SIGNED (lresize'length-1 downto 0);
  begin  -- ?>=
    if ((L'length < 1) or (R'length < 1)) then
      assert no_warning
        report fixed_generic_pkg'instance_name
        & """?>="": null detected, returning X"
        severity warning;
      return 'X';
    else
      lresize := resize (L, left_index, right_index);
      rresize := resize (R, left_index, right_index);
      lslv    := to_s (lresize);
      rslv    := to_s (rresize);
      return lslv ?>= rslv;
    end if;
  end function "?>=";

  function "?<" (L, R : UNRESOLVED_sfixed) return STD_ULOGIC is
    constant left_index       : INTEGER := maximum(L'high, R'high);
    constant right_index      : INTEGER := mins(L'low, R'low);
    variable lresize, rresize : UNRESOLVED_sfixed (left_index downto right_index);
    variable lslv, rslv       : UNRESOLVED_SIGNED (lresize'length-1 downto 0);
  begin  -- ?<
    if ((L'length < 1) or (R'length < 1)) then
      assert no_warning
        report fixed_generic_pkg'instance_name
        & """?<"": null detected, returning X"
        severity warning;
      return 'X';
    else
      lresize := resize (L, left_index, right_index);
      rresize := resize (R, left_index, right_index);
      lslv    := to_s (lresize);
      rslv    := to_s (rresize);
      return lslv ?< rslv;
    end if;
  end function "?<";

  function "?<=" (L, R : UNRESOLVED_sfixed) return STD_ULOGIC is
    constant left_index       : INTEGER := maximum(L'high, R'high);
    constant right_index      : INTEGER := mins(L'low, R'low);
    variable lresize, rresize : UNRESOLVED_sfixed (left_index downto right_index);
    variable lslv, rslv       : UNRESOLVED_SIGNED (lresize'length-1 downto 0);
  begin  -- ?<=
    if ((L'length < 1) or (R'length < 1)) then
      assert no_warning
        report fixed_generic_pkg'instance_name
        & """?<="": null detected, returning X"
        severity warning;
      return 'X';
    else
      lresize := resize (L, left_index, right_index);
      rresize := resize (R, left_index, right_index);
      lslv    := to_s (lresize);
      rslv    := to_s (rresize);
      return lslv ?<= rslv;
    end if;
  end function "?<=";

  -- Match function, similar to "std_match" from numeric_std
  function std_match (L, R : UNRESOLVED_ufixed) return BOOLEAN is
  begin
    if (L'high = R'high and L'low = R'low) then
      return std_match(to_sulv(L), to_sulv(R));
    else
      assert no_warning
        report fixed_generic_pkg'instance_name
        & "STD_MATCH: L'RANGE /= R'RANGE, returning FALSE"
        severity warning;
      return false;
    end if;
  end function std_match;

  function std_match (L, R : UNRESOLVED_sfixed) return BOOLEAN is
  begin
    if (L'high = R'high and L'low = R'low) then
      return std_match(to_sulv(L), to_sulv(R));
    else
      assert no_warning
        report fixed_generic_pkg'instance_name
        & "STD_MATCH: L'RANGE /= R'RANGE, returning FALSE"
        severity warning;
      return false;
    end if;
  end function std_match;

  -- compare functions
  function "=" (
    l, r : UNRESOLVED_ufixed)           -- fixed point input
    return BOOLEAN
  is
    constant left_index       : INTEGER := maximum(l'high, r'high);
    constant right_index      : INTEGER := mins(l'low, r'low);
    variable lresize, rresize : UNRESOLVED_ufixed (left_index downto right_index);
    variable lslv, rslv       : UNRESOLVED_UNSIGNED (lresize'length-1 downto 0);
  begin
    if (l'length < 1 or r'length < 1) then
      assert no_warning
        report fixed_generic_pkg'instance_name
        & """="": null argument detected, returning FALSE"
        severity warning;
      return false;
    elsif (Is_X(l) or Is_X(r)) then
      assert no_warning
        report fixed_generic_pkg'instance_name
        & """="": metavalue detected, returning FALSE"
        severity warning;
      return false;
    end if;
    lresize := resize (l, left_index, right_index);
    rresize := resize (r, left_index, right_index);
    lslv    := to_uns (lresize);
    rslv    := to_uns (rresize);
    return lslv = rslv;
  end function "=";

  function "=" (
    l, r : UNRESOLVED_sfixed)           -- fixed point input
    return BOOLEAN
  is
    constant left_index       : INTEGER := maximum(l'high, r'high);
    constant right_index      : INTEGER := mins(l'low, r'low);
    variable lresize, rresize : UNRESOLVED_sfixed (left_index downto right_index);
    variable lslv, rslv       : UNRESOLVED_SIGNED (lresize'length-1 downto 0);
  begin
    if (l'length < 1 or r'length < 1) then
      assert no_warning
        report fixed_generic_pkg'instance_name
        & """="": null argument detected, returning FALSE"
        severity warning;
      return false;
    elsif (Is_X(l) or Is_X(r)) then
      assert no_warning
        report fixed_generic_pkg'instance_name
        & """="": metavalue detected, returning FALSE"
        severity warning;
      return false;
    end if;
    lresize := resize (l, left_index, right_index);
    rresize := resize (r, left_index, right_index);
    lslv    := to_s (lresize);
    rslv    := to_s (rresize);
    return lslv = rslv;
  end function "=";

  function "/=" (
    l, r : UNRESOLVED_ufixed)           -- fixed point input
    return BOOLEAN
  is
    constant left_index       : INTEGER := maximum(l'high, r'high);
    constant right_index      : INTEGER := mins(l'low, r'low);
    variable lresize, rresize : UNRESOLVED_ufixed (left_index downto right_index);
    variable lslv, rslv       : UNRESOLVED_UNSIGNED (lresize'length-1 downto 0);
  begin
    if (l'length < 1 or r'length < 1) then
      assert no_warning
        report fixed_generic_pkg'instance_name
        & """/="": null argument detected, returning TRUE"
        severity warning;
      return true;
    elsif (Is_X(l) or Is_X(r)) then
      assert no_warning
        report fixed_generic_pkg'instance_name
        & """/="": metavalue detected, returning TRUE"
        severity warning;
      return true;
    end if;
    lresize := resize (l, left_index, right_index);
    rresize := resize (r, left_index, right_index);
    lslv    := to_uns (lresize);
    rslv    := to_uns (rresize);
    return lslv /= rslv;
  end function "/=";

  function "/=" (
    l, r : UNRESOLVED_sfixed)           -- fixed point input
    return BOOLEAN
  is
    constant left_index       : INTEGER := maximum(l'high, r'high);
    constant right_index      : INTEGER := mins(l'low, r'low);
    variable lresize, rresize : UNRESOLVED_sfixed (left_index downto right_index);
    variable lslv, rslv       : UNRESOLVED_SIGNED (lresize'length-1 downto 0);
  begin
    if (l'length < 1 or r'length < 1) then
      assert no_warning
        report fixed_generic_pkg'instance_name
        & """/="": null argument detected, returning TRUE"
        severity warning;
      return true;
    elsif (Is_X(l) or Is_X(r)) then
      assert no_warning
        report fixed_generic_pkg'instance_name
        & """/="": metavalue detected, returning TRUE"
        severity warning;
      return true;
    end if;
    lresize := resize (l, left_index, right_index);
    rresize := resize (r, left_index, right_index);
    lslv    := to_s (lresize);
    rslv    := to_s (rresize);
    return lslv /= rslv;
  end function "/=";

  function ">" (
    l, r : UNRESOLVED_ufixed)           -- fixed point input
    return BOOLEAN
  is
    constant left_index       : INTEGER := maximum(l'high, r'high);
    constant right_index      : INTEGER := mins(l'low, r'low);
    variable lresize, rresize : UNRESOLVED_ufixed (left_index downto right_index);
    variable lslv, rslv       : UNRESOLVED_UNSIGNED (lresize'length-1 downto 0);
  begin
    if (l'length < 1 or r'length < 1) then
      assert no_warning
        report fixed_generic_pkg'instance_name
        & """>"": null argument detected, returning FALSE"
        severity warning;
      return false;
    elsif (Is_X(l) or Is_X(r)) then
      assert no_warning
        report fixed_generic_pkg'instance_name
        & """>"": metavalue detected, returning FALSE"
        severity warning;
      return false;
    end if;
    lresize := resize (l, left_index, right_index);
    rresize := resize (r, left_index, right_index);
    lslv    := to_uns (lresize);
    rslv    := to_uns (rresize);
    return lslv > rslv;
  end function ">";

  function ">" (
    l, r : UNRESOLVED_sfixed)           -- fixed point input
    return BOOLEAN
  is
    constant left_index       : INTEGER := maximum(l'high, r'high);
    constant right_index      : INTEGER := mins(l'low, r'low);
    variable lresize, rresize : UNRESOLVED_sfixed (left_index downto right_index);
    variable lslv, rslv       : UNRESOLVED_SIGNED (lresize'length-1 downto 0);
  begin
    if (l'length < 1 or r'length < 1) then
      assert no_warning
        report fixed_generic_pkg'instance_name
        & """>"": null argument detected, returning FALSE"
        severity warning;
      return false;
    elsif (Is_X(l) or Is_X(r)) then
      assert no_warning
        report fixed_generic_pkg'instance_name
        & """>"": metavalue detected, returning FALSE"
        severity warning;
      return false;
    end if;
    lresize := resize (l, left_index, right_index);
    rresize := resize (r, left_index, right_index);
    lslv    := to_s (lresize);
    rslv    := to_s (rresize);
    return lslv > rslv;
  end function ">";

  function "<" (
    l, r : UNRESOLVED_ufixed)           -- fixed point input
    return BOOLEAN
  is
    constant left_index       : INTEGER := maximum(l'high, r'high);
    constant right_index      : INTEGER := mins(l'low, r'low);
    variable lresize, rresize : UNRESOLVED_ufixed (left_index downto right_index);
    variable lslv, rslv       : UNRESOLVED_UNSIGNED (lresize'length-1 downto 0);
  begin
    if (l'length < 1 or r'length < 1) then
      assert no_warning
        report fixed_generic_pkg'instance_name
        & """<"": null argument detected, returning FALSE"
        severity warning;
      return false;
    elsif (Is_X(l) or Is_X(r)) then
      assert no_warning
        report fixed_generic_pkg'instance_name
        & """<"": metavalue detected, returning FALSE"
        severity warning;
      return false;
    end if;
    lresize := resize (l, left_index, right_index);
    rresize := resize (r, left_index, right_index);
    lslv    := to_uns (lresize);
    rslv    := to_uns (rresize);
    return lslv < rslv;
  end function "<";

  function "<" (
    l, r : UNRESOLVED_sfixed)           -- fixed point input
    return BOOLEAN
  is
    constant left_index       : INTEGER := maximum(l'high, r'high);
    constant right_index      : INTEGER := mins(l'low, r'low);
    variable lresize, rresize : UNRESOLVED_sfixed (left_index downto right_index);
    variable lslv, rslv       : UNRESOLVED_SIGNED (lresize'length-1 downto 0);
  begin
    if (l'length < 1 or r'length < 1) then
      assert no_warning
        report fixed_generic_pkg'instance_name
        & """<"": null argument detected, returning FALSE"
        severity warning;
      return false;
    elsif (Is_X(l) or Is_X(r)) then
      assert no_warning
        report fixed_generic_pkg'instance_name
        & """<"": metavalue detected, returning FALSE"
        severity warning;
      return false;
    end if;
    lresize := resize (l, left_index, right_index);
    rresize := resize (r, left_index, right_index);
    lslv    := to_s (lresize);
    rslv    := to_s (rresize);
    return lslv < rslv;
  end function "<";

  function ">=" (
    l, r : UNRESOLVED_ufixed)           -- fixed point input
    return BOOLEAN
  is
    constant left_index       : INTEGER := maximum(l'high, r'high);
    constant right_index      : INTEGER := mins(l'low, r'low);
    variable lresize, rresize : UNRESOLVED_ufixed (left_index downto right_index);
    variable lslv, rslv       : UNRESOLVED_UNSIGNED (lresize'length-1 downto 0);
  begin
    if (l'length < 1 or r'length < 1) then
      assert no_warning
        report fixed_generic_pkg'instance_name
        & """>="": null argument detected, returning FALSE"
        severity warning;
      return false;
    elsif (Is_X(l) or Is_X(r)) then
      assert no_warning
        report fixed_generic_pkg'instance_name
        & """>="": metavalue detected, returning FALSE"
        severity warning;
      return false;
    end if;
    lresize := resize (l, left_index, right_index);
    rresize := resize (r, left_index, right_index);
    lslv    := to_uns (lresize);
    rslv    := to_uns (rresize);
    return lslv >= rslv;
  end function ">=";

  function ">=" (
    l, r : UNRESOLVED_sfixed)           -- fixed point input
    return BOOLEAN
  is
    constant left_index       : INTEGER := maximum(l'high, r'high);
    constant right_index      : INTEGER := mins(l'low, r'low);
    variable lresize, rresize : UNRESOLVED_sfixed (left_index downto right_index);
    variable lslv, rslv       : UNRESOLVED_SIGNED (lresize'length-1 downto 0);
  begin
    if (l'length < 1 or r'length < 1) then
      assert no_warning
        report fixed_generic_pkg'instance_name
        & """>="": null argument detected, returning FALSE"
        severity warning;
      return false;
    elsif (Is_X(l) or Is_X(r)) then
      assert no_warning
        report fixed_generic_pkg'instance_name
        & """>="": metavalue detected, returning FALSE"
        severity warning;
      return false;
    end if;
    lresize := resize (l, left_index, right_index);
    rresize := resize (r, left_index, right_index);
    lslv    := to_s (lresize);
    rslv    := to_s (rresize);
    return lslv >= rslv;
  end function ">=";

  function "<=" (
    l, r : UNRESOLVED_ufixed)           -- fixed point input
    return BOOLEAN
  is
    constant left_index       : INTEGER := maximum(l'high, r'high);
    constant right_index      : INTEGER := mins(l'low, r'low);
    variable lresize, rresize : UNRESOLVED_ufixed (left_index downto right_index);
    variable lslv, rslv       : UNRESOLVED_UNSIGNED (lresize'length-1 downto 0);
  begin
    if (l'length < 1 or r'length < 1) then
      assert no_warning
        report fixed_generic_pkg'instance_name
        & """<="": null argument detected, returning FALSE"
        severity warning;
      return false;
    elsif (Is_X(l) or Is_X(r)) then
      assert no_warning
        report fixed_generic_pkg'instance_name
        & """<="": metavalue detected, returning FALSE"
        severity warning;
      return false;
    end if;
    lresize     := resize (l, left_index, right_index);
    rresize     := resize (r, left_index, right_index);
    lslv        := to_uns (lresize);
    rslv        := to_uns (rresize);
    return lslv <= rslv;
  end function "<=";

  function "<=" (
    l, r : UNRESOLVED_sfixed)           -- fixed point input
    return BOOLEAN
  is
    constant left_index       : INTEGER := maximum(l'high, r'high);
    constant right_index      : INTEGER := mins(l'low, r'low);
    variable lresize, rresize : UNRESOLVED_sfixed (left_index downto right_index);
    variable lslv, rslv       : UNRESOLVED_SIGNED (lresize'length-1 downto 0);
  begin
    if (l'length < 1 or r'length < 1) then
      assert no_warning
        report fixed_generic_pkg'instance_name
        & """<="": null argument detected, returning FALSE"
        severity warning;
      return false;
    elsif (Is_X(l) or Is_X(r)) then
      assert no_warning
        report fixed_generic_pkg'instance_name
        & """<="": metavalue detected, returning FALSE"
        severity warning;
      return false;
    end if;
    lresize     := resize (l, left_index, right_index);
    rresize     := resize (r, left_index, right_index);
    lslv        := to_s (lresize);
    rslv        := to_s (rresize);
    return lslv <= rslv;
  end function "<=";

  -- overloads of the default maximum and minimum functions
  function maximum (l, r : UNRESOLVED_ufixed) return UNRESOLVED_ufixed is
    constant left_index       : INTEGER := maximum(l'high, r'high);
    constant right_index      : INTEGER := mins(l'low, r'low);
    variable lresize, rresize : UNRESOLVED_ufixed (left_index downto right_index);
  begin
    if (l'length < 1 or r'length < 1) then
      return NAUF;
    end if;
    lresize := resize (l, left_index, right_index);
    rresize := resize (r, left_index, right_index);
    return to_fixed(maximum(to_uns(lresize), to_uns(rresize)),
                    left_index, right_index);
  end function maximum;

  function maximum (l, r : UNRESOLVED_sfixed) return UNRESOLVED_sfixed is
    constant left_index       : INTEGER := maximum(l'high, r'high);
    constant right_index      : INTEGER := mins(l'low, r'low);
    variable lresize, rresize : UNRESOLVED_sfixed (left_index downto right_index);
  begin
    if (l'length < 1 or r'length < 1) then
      return NASF;
    end if;
    lresize := resize (l, left_index, right_index);
    rresize := resize (r, left_index, right_index);
    return to_fixed(maximum(to_s(lresize), to_s(rresize)),
                    left_index, right_index);
  end function maximum;

  function minimum (l, r : UNRESOLVED_ufixed) return UNRESOLVED_ufixed is
    constant left_index       : INTEGER := maximum(l'high, r'high);
    constant right_index      : INTEGER := mins(l'low, r'low);
    variable lresize, rresize : UNRESOLVED_ufixed (left_index downto right_index);
  begin
    if (l'length < 1 or r'length < 1) then
      return NAUF;
    end if;
    lresize := resize (l, left_index, right_index);
    rresize := resize (r, left_index, right_index);
    return to_fixed(minimum(to_uns(lresize), to_uns(rresize)),
                    left_index, right_index);
  end function minimum;

  function minimum (l, r : UNRESOLVED_sfixed) return UNRESOLVED_sfixed is
    constant left_index       : INTEGER := maximum(l'high, r'high);
    constant right_index      : INTEGER := mins(l'low, r'low);
    variable lresize, rresize : UNRESOLVED_sfixed (left_index downto right_index);
  begin
    if (l'length < 1 or r'length < 1) then
      return NASF;
    end if;
    lresize := resize (l, left_index, right_index);
    rresize := resize (r, left_index, right_index);
    return to_fixed(minimum(to_s(lresize), to_s(rresize)),
                    left_index, right_index);
  end function minimum;

  function to_ufixed (
    arg                     : NATURAL;  -- integer
    constant left_index     : INTEGER;  -- left index (high index)
    constant right_index    : INTEGER                   := 0;  -- right index
    constant overflow_style : fixed_overflow_style_type := fixed_overflow_style;
    constant round_style    : fixed_round_style_type    := fixed_round_style)
    return UNRESOLVED_ufixed
  is
    constant fw      : INTEGER := mins (right_index, right_index);  -- catch literals
    variable result  : UNRESOLVED_ufixed (left_index downto fw);
    variable sresult : UNRESOLVED_ufixed (left_index downto 0) :=
      (others => '0');  -- integer portion
    variable argx    : NATURAL;         -- internal version of arg
  begin
    if (result'length < 1) then
      return NAUF;
    end if;
    if arg /= 0 then
      argx := arg;
      for I in 0 to sresult'left loop
        if (argx mod 2) = 0 then
          sresult(I) := '0';
        else
          sresult(I) := '1';
        end if;
        argx := argx/2;
      end loop;
      if argx /= 0 then
        assert no_warning
          report fixed_generic_pkg'instance_name
          & "TO_UFIXED(NATURAL): vector truncated"
          severity warning;
        if overflow_style = fixed_saturate then
          return saturate (left_index, right_index);
        end if;
      end if;
      result := resize (arg            => sresult,
                        left_index     => left_index,
                        right_index    => right_index,
                        round_style    => round_style,
                        overflow_style => overflow_style);
    else
      result := (others => '0');
    end if;
    return result;
  end function to_ufixed;

  function to_sfixed (
    arg                     : INTEGER;  -- integer
    constant left_index     : INTEGER;  -- left index (high index)
    constant right_index    : INTEGER                   := 0;  -- right index
    constant overflow_style : fixed_overflow_style_type := fixed_overflow_style;
    constant round_style    : fixed_round_style_type    := fixed_round_style)
    return UNRESOLVED_sfixed
  is
    constant fw      : INTEGER := mins (right_index, right_index);  -- catch literals
    variable result  : UNRESOLVED_sfixed (left_index downto fw);
    variable sresult : UNRESOLVED_sfixed (left_index downto 0) :=
      (others => '0');  -- integer portion
    variable argx    : INTEGER;         -- internal version of arg
    variable sign    : STD_ULOGIC;      -- sign of input
  begin
    if (result'length < 1) then         -- null range
      return NASF;
    end if;
    if arg /= 0 then
      if (arg < 0) then
        sign := '1';
        argx := -(arg + 1);
      else
        sign := '0';
        argx := arg;
      end if;
      for I in 0 to sresult'left loop
        if (argx mod 2) = 0 then
          sresult(I) := sign;
        else
          sresult(I) := not sign;
        end if;
        argx := argx/2;
      end loop;
      if argx /= 0 or left_index < 0 or sign /= sresult(sresult'left) then
        assert no_warning
          report fixed_generic_pkg'instance_name
          & "TO_SFIXED(INTEGER): vector truncated"
          severity warning;
        if overflow_style = fixed_saturate then                -- saturate
          if arg < 0 then
            result := not saturate (result'high, result'low);  -- underflow
          else
            result := saturate (result'high, result'low);      -- overflow
          end if;
          return result;
        end if;
      end if;
      result := resize (arg            => sresult,
                        left_index     => left_index,
                        right_index    => right_index,
                        round_style    => round_style,
                        overflow_style => overflow_style);
    else
      result := (others => '0');
    end if;
    return result;
  end function to_sfixed;

  function to_ufixed (
    arg                     : REAL;     -- real
    constant left_index     : INTEGER;  -- left index (high index)
    constant right_index    : INTEGER;  -- right index
    constant overflow_style : fixed_overflow_style_type := fixed_overflow_style;
    constant round_style    : fixed_round_style_type    := fixed_round_style;
    constant guard_bits     : NATURAL                   := fixed_guard_bits)  -- # of guard bits
    return UNRESOLVED_ufixed
  is
    constant fw              : INTEGER := mins (right_index, right_index);  -- catch literals
    variable result          : UNRESOLVED_ufixed (left_index downto fw) :=
      (others => '0');
    variable Xresult         : UNRESOLVED_ufixed (left_index downto
                                                  fw-guard_bits) :=
      (others => '0');
    variable presult         : REAL;
  begin
    -- If negative or null range, return.
    if (left_index < fw) then
      return NAUF;
    end if;
    if (arg < 0.0) then
      report fixed_generic_pkg'instance_name
        & "TO_UFIXED: Negative argument passed "
        & REAL'image(arg) severity error;
      return result;
    end if;
    presult := arg;
    if presult >= (2.0**(left_index+1)) then
      assert no_warning report fixed_generic_pkg'instance_name
        & "TO_UFIXED(REAL): vector truncated"
        severity warning;
      if overflow_style = fixed_wrap then
        presult := presult mod (2.0**(left_index+1));  -- wrap
      else
        return saturate (result'high, result'low);
      end if;
    end if;
    for i in Xresult'range loop
      if presult >= 2.0**i then
        Xresult(i) := '1';
        presult    := presult - 2.0**i;
      else
        Xresult(i) := '0';
      end if;
    end loop;
    if guard_bits > 0 and round_style = fixed_round then
      result := round_fixed (arg => Xresult (left_index
                                             downto right_index),
                             remainder => Xresult (right_index-1 downto
                                                   right_index-guard_bits),
                             overflow_style => overflow_style);
    else
      result := Xresult (result'range);
    end if;
    return result;
  end function to_ufixed;

  function to_sfixed (
    arg                     : REAL;     -- real
    constant left_index     : INTEGER;  -- left index (high index)
    constant right_index    : INTEGER;  -- right index
    constant overflow_style : fixed_overflow_style_type := fixed_overflow_style;
    constant round_style    : fixed_round_style_type    := fixed_round_style;
    constant guard_bits     : NATURAL                   := fixed_guard_bits)  -- # of guard bits
    return UNRESOLVED_sfixed
  is
    constant fw     : INTEGER := mins (right_index, right_index);  -- catch literals
    variable result : UNRESOLVED_sfixed (left_index downto fw) :=
      (others => '0');
    variable Xresult : UNRESOLVED_sfixed (left_index+1 downto fw-guard_bits) :=
      (others => '0');
    variable presult : REAL;
  begin
    if (left_index < fw) then           -- null range
      return NASF;
    end if;
    if (arg >= (2.0**left_index) or arg < -(2.0**left_index)) then
      assert no_warning report fixed_generic_pkg'instance_name
        & "TO_SFIXED(REAL): vector truncated"
        severity warning;
      if overflow_style = fixed_saturate then
        if arg < 0.0 then               -- saturate
          result := not saturate (result'high, result'low);        -- underflow
        else
          result := saturate (result'high, result'low);            -- overflow
        end if;
        return result;
      else
        presult := abs(arg) mod (2.0**(left_index+1));             -- wrap
      end if;
    else
      presult := abs(arg);
    end if;
    for i in Xresult'range loop
      if presult >= 2.0**i then
        Xresult(i) := '1';
        presult    := presult - 2.0**i;
      else
        Xresult(i) := '0';
      end if;
    end loop;
    if arg < 0.0 then
      Xresult := to_fixed(-to_s(Xresult), Xresult'high, Xresult'low);
    end if;
    if guard_bits > 0 and round_style = fixed_round then
      result := round_fixed (arg => Xresult (left_index
                                             downto right_index),
                             remainder => Xresult (right_index-1 downto
                                                   right_index-guard_bits),
                             overflow_style => overflow_style);
    else
      result := Xresult (result'range);
    end if;
    return result;
  end function to_sfixed;

  function to_ufixed (
    arg                     : UNRESOLVED_UNSIGNED;             -- unsigned
    constant left_index     : INTEGER;  -- left index (high index)
    constant right_index    : INTEGER                   := 0;  -- right index
    constant overflow_style : fixed_overflow_style_type := fixed_overflow_style;
    constant round_style    : fixed_round_style_type    := fixed_round_style)
    return UNRESOLVED_ufixed
  is
    constant ARG_LEFT : INTEGER := arg'length-1;
    alias XARG        : UNRESOLVED_UNSIGNED(ARG_LEFT downto 0) is arg;
    variable result   : UNRESOLVED_ufixed (left_index downto right_index);
  begin
    if arg'length < 1 or (left_index < right_index) then
      return NAUF;
    end if;
    result := resize (arg            => UNRESOLVED_ufixed (XARG),
                      left_index     => left_index,
                      right_index    => right_index,
                      round_style    => round_style,
                      overflow_style => overflow_style);
    return result;
  end function to_ufixed;

  -- converted version
  function to_ufixed (
    arg : UNRESOLVED_UNSIGNED)          -- unsigned
    return UNRESOLVED_ufixed
  is
    constant ARG_LEFT : INTEGER := arg'length-1;
    alias XARG        : UNRESOLVED_UNSIGNED(ARG_LEFT downto 0) is arg;
  begin
    if arg'length < 1 then
      return NAUF;
    end if;
    return UNRESOLVED_ufixed(XARG);
  end function to_ufixed;

  function to_sfixed (
    arg                     : UNRESOLVED_SIGNED;               -- signed
    constant left_index     : INTEGER;  -- left index (high index)
    constant right_index    : INTEGER                   := 0;  -- right index
    constant overflow_style : fixed_overflow_style_type := fixed_overflow_style;
    constant round_style    : fixed_round_style_type    := fixed_round_style)
    return UNRESOLVED_sfixed
  is
    constant ARG_LEFT : INTEGER := arg'length-1;
    alias XARG        : UNRESOLVED_SIGNED(ARG_LEFT downto 0) is arg;
    variable result   : UNRESOLVED_sfixed (left_index downto right_index);
  begin
    if arg'length < 1 or (left_index < right_index) then
      return NASF;
    end if;
    result := resize (arg            => UNRESOLVED_sfixed (XARG),
                      left_index     => left_index,
                      right_index    => right_index,
                      round_style    => round_style,
                      overflow_style => overflow_style);
    return result;
  end function to_sfixed;

  -- converted version
  function to_sfixed (
    arg : UNRESOLVED_SIGNED)            -- signed
    return UNRESOLVED_sfixed
  is
    constant ARG_LEFT : INTEGER := arg'length-1;
    alias XARG        : UNRESOLVED_SIGNED(ARG_LEFT downto 0) is arg;
  begin
    if arg'length < 1 then
      return NASF;
    end if;
    return UNRESOLVED_sfixed(XARG);
  end function to_sfixed;

  function to_sfixed (arg : UNRESOLVED_ufixed) return UNRESOLVED_sfixed is
    variable result : UNRESOLVED_sfixed (arg'high+1 downto arg'low);
  begin
    if arg'length < 1 then
      return NASF;
    end if;
    result (arg'high downto arg'low) := UNRESOLVED_sfixed(cleanvec(arg));
    result (arg'high+1)              := '0';
    return result;
  end function to_sfixed;

  -- Because of the fairly complicated sizing rules in the fixed point
  -- packages these functions are provided to compute the result ranges
  -- Example:
  -- signal uf1 : ufixed (3 downto -3);
  -- signal uf2 : ufixed (4 downto -2);
  -- signal uf1multuf2 : ufixed (ufixed_high (3, -3, '*', 4, -2) downto
  --                             ufixed_low (3, -3, '*', 4, -2));
  -- uf1multuf2 <= uf1 * uf2;
  -- Valid characters: '+', '-', '*', '/', 'r' or 'R' (rem), 'm' or 'M' (mod),
  -- '1' (reciprocal), 'A', 'a' (abs), 'N', 'n' (-sfixed)
  function ufixed_high (left_index, right_index   : INTEGER;
                        operation                 : CHARACTER := 'X';
                        left_index2, right_index2 : INTEGER   := 0)
    return INTEGER is
  begin
    case operation is
      when '+'| '-' => return maximum (left_index, left_index2) + 1;
      when '*'      => return left_index + left_index2 + 1;
      when '/'      => return left_index - right_index2;
      when '1'      => return -right_index;                    -- reciprocal
      when 'R'|'r'  => return mins (left_index, left_index2);  -- "rem"
      when 'M'|'m'  => return mins (left_index, left_index2);  -- "mod"
      when others   => return left_index;  -- For abs and default
    end case;
  end function ufixed_high;

  function ufixed_low (left_index, right_index   : INTEGER;
                       operation                 : CHARACTER := 'X';
                       left_index2, right_index2 : INTEGER   := 0)
    return INTEGER is
  begin
    case operation is
      when '+'| '-' => return mins (right_index, right_index2);
      when '*'      => return right_index + right_index2;
      when '/'      => return right_index - left_index2 - 1;
      when '1'      => return -left_index - 1;                   -- reciprocal
      when 'R'|'r'  => return mins (right_index, right_index2);  -- "rem"
      when 'M'|'m'  => return mins (right_index, right_index2);  -- "mod"
      when others   => return right_index;  -- for abs and default
    end case;
  end function ufixed_low;

  function sfixed_high (left_index, right_index   : INTEGER;
                        operation                 : CHARACTER := 'X';
                        left_index2, right_index2 : INTEGER   := 0)
    return INTEGER is
  begin
    case operation is
      when '+'| '-' => return maximum (left_index, left_index2) + 1;
      when '*'      => return left_index + left_index2 + 1;
      when '/'      => return left_index - right_index2 + 1;
      when '1'      => return -right_index + 1;                -- reciprocal
      when 'R'|'r'  => return mins (left_index, left_index2);  -- "rem"
      when 'M'|'m'  => return left_index2;                     -- "mod"
      when 'A'|'a'  => return left_index + 1;                  -- "abs"
      when 'N'|'n'  => return left_index + 1;                  -- -sfixed
      when others   => return left_index;
    end case;
  end function sfixed_high;

  function sfixed_low (left_index, right_index   : INTEGER;
                       operation                 : CHARACTER := 'X';
                       left_index2, right_index2 : INTEGER   := 0)
    return INTEGER is
  begin
    case operation is
      when '+'| '-' => return mins (right_index, right_index2);
      when '*'      => return right_index + right_index2;
      when '/'      => return right_index - left_index2;
      when '1'      => return -left_index;  -- reciprocal
      when 'R'|'r'  => return mins (right_index, right_index2);  -- "rem"
      when 'M'|'m'  => return mins (right_index, right_index2);  -- "mod"
      when others   => return right_index;  -- default for abs, neg and default
    end case;
  end function sfixed_low;

  -- Same as above, but using the "size_res" input only for their ranges:
  -- signal uf1multuf2 : ufixed (ufixed_high (uf1, '*', uf2) downto
  --                             ufixed_low (uf1, '*', uf2));
  -- uf1multuf2 <= uf1 * uf2;
  function ufixed_high (size_res  : UNRESOLVED_ufixed;
                        operation : CHARACTER := 'X';
                        size_res2 : UNRESOLVED_ufixed)
    return INTEGER is
  begin
    return ufixed_high (left_index   => size_res'high,
                        right_index  => size_res'low,
                        operation    => operation,
                        left_index2  => size_res2'high,
                        right_index2 => size_res2'low);
  end function ufixed_high;

  function ufixed_low (size_res  : UNRESOLVED_ufixed;
                       operation : CHARACTER := 'X';
                       size_res2 : UNRESOLVED_ufixed)
    return INTEGER is
  begin
    return ufixed_low (left_index   => size_res'high,
                       right_index  => size_res'low,
                       operation    => operation,
                       left_index2  => size_res2'high,
                       right_index2 => size_res2'low);
  end function ufixed_low;

  function sfixed_high (size_res  : UNRESOLVED_sfixed;
                        operation : CHARACTER := 'X';
                        size_res2 : UNRESOLVED_sfixed)
    return INTEGER is
  begin
    return sfixed_high (left_index   => size_res'high,
                        right_index  => size_res'low,
                        operation    => operation,
                        left_index2  => size_res2'high,
                        right_index2 => size_res2'low);
  end function sfixed_high;

  function sfixed_low (size_res  : UNRESOLVED_sfixed;
                       operation : CHARACTER := 'X';
                       size_res2 : UNRESOLVED_sfixed)
    return INTEGER is
  begin
    return sfixed_low (left_index   => size_res'high,
                       right_index  => size_res'low,
                       operation    => operation,
                       left_index2  => size_res2'high,
                       right_index2 => size_res2'low);
  end function sfixed_low;

  -- purpose: returns a saturated number
  function saturate (
    constant left_index  : INTEGER;
    constant right_index : INTEGER)
    return UNRESOLVED_ufixed
  is
    constant sat : UNRESOLVED_ufixed (left_index downto right_index) :=
      (others => '1');
  begin
    return sat;
  end function saturate;

  -- purpose: returns a saturated number
  function saturate (
    constant left_index  : INTEGER;
    constant right_index : INTEGER)
    return UNRESOLVED_sfixed
  is
    variable sat : UNRESOLVED_sfixed (left_index downto right_index) :=
      (others => '1');
  begin
    -- saturate positive, to saturate negative, just do "not saturate()"
    sat (left_index) := '0';
    return sat;
  end function saturate;

  function saturate (
    size_res : UNRESOLVED_ufixed)       -- only the size of this is used
    return UNRESOLVED_ufixed is
  begin
    return saturate (size_res'high, size_res'low);
  end function saturate;

  function saturate (
    size_res : UNRESOLVED_sfixed)       -- only the size of this is used
    return UNRESOLVED_sfixed is
  begin
    return saturate (size_res'high, size_res'low);
  end function saturate;

  -- As a concession to those who use a graphical DSP environment,
  -- these functions take parameters in those tools format and create
  -- fixed point numbers.  These functions are designed to convert from
  -- a std_logic_vector to the VHDL fixed point format using the conventions
  -- of these packages.  In a pure VHDL environment you should use the
  -- "to_ufixed" and "to_sfixed" routines.
  -- Unsigned fixed point
  function to_UFix (
    arg      : STD_ULOGIC_VECTOR;
    width    : NATURAL;                 -- width of vector
    fraction : NATURAL)                 -- width of fraction
    return UNRESOLVED_ufixed
  is
    variable result : UNRESOLVED_ufixed (width-fraction-1 downto -fraction);
  begin
    if (arg'length /= result'length) then
      report fixed_generic_pkg'instance_name
        & "TO_UFIX (STD_ULOGIC_VECTOR) "
        & "Vector lengths do not match.  Input length is "
        & INTEGER'image(arg'length) & " and output will be "
        & INTEGER'image(result'length) & " wide."
        severity error;
      return NAUF;
    else
      result := to_ufixed (arg, result'high, result'low);
      return result;
    end if;
  end function to_UFix;

  -- signed fixed point
  function to_SFix (
    arg      : STD_ULOGIC_VECTOR;
    width    : NATURAL;                 -- width of vector
    fraction : NATURAL)                 -- width of fraction
    return UNRESOLVED_sfixed
  is
    variable result : UNRESOLVED_sfixed (width-fraction-1 downto -fraction);
  begin
    if (arg'length /= result'length) then
      report fixed_generic_pkg'instance_name
        & "TO_SFIX (STD_ULOGIC_VECTOR) "
        & "Vector lengths do not match.  Input length is "
        & INTEGER'image(arg'length) & " and output will be "
        & INTEGER'image(result'length) & " wide."
        severity error;
      return NASF;
    else
      result := to_sfixed (arg, result'high, result'low);
      return result;
    end if;
  end function to_SFix;

  -- finding the bounds of a number.  These functions can be used like this:
  -- signal xxx : ufixed (7 downto -3);
  -- -- Which is the same as "ufixed (UFix_high (11,3) downto UFix_low(11,3))"
  -- signal yyy : ufixed (UFix_high (11, 3, "+", 11, 3)
  --               downto UFix_low(11, 3, "+", 11, 3));
  -- Where "11" is the width of xxx (xxx'length),
  -- and 3 is the lower bound (abs (xxx'low))
  -- In a pure VHDL environment use "ufixed_high" and "ufixed_low"
  function ufix_high (
    width, fraction   : NATURAL;
    operation         : CHARACTER := 'X';
    width2, fraction2 : NATURAL   := 0)
    return INTEGER is
  begin
    return ufixed_high (left_index   => width - 1 - fraction,
                        right_index  => -fraction,
                        operation    => operation,
                        left_index2  => width2 - 1 - fraction2,
                        right_index2 => -fraction2);
  end function ufix_high;

  function ufix_low (
    width, fraction   : NATURAL;
    operation         : CHARACTER := 'X';
    width2, fraction2 : NATURAL   := 0)
    return INTEGER is
  begin
    return ufixed_low (left_index   => width - 1 - fraction,
                       right_index  => -fraction,
                       operation    => operation,
                       left_index2  => width2 - 1 - fraction2,
                       right_index2 => -fraction2);
  end function ufix_low;

  function sfix_high (
    width, fraction   : NATURAL;
    operation         : CHARACTER := 'X';
    width2, fraction2 : NATURAL   := 0)
    return INTEGER is
  begin
    return sfixed_high (left_index   => width - fraction,
                        right_index  => -fraction,
                        operation    => operation,
                        left_index2  => width2 - fraction2,
                        right_index2 => -fraction2);
  end function sfix_high;

  function sfix_low (
    width, fraction   : NATURAL;
    operation         : CHARACTER := 'X';
    width2, fraction2 : NATURAL   := 0)
    return INTEGER is
  begin
    return sfixed_low (left_index   => width - fraction,
                       right_index  => -fraction,
                       operation    => operation,
                       left_index2  => width2 - fraction2,
                       right_index2 => -fraction2);
  end function sfix_low;

  function to_unsigned (
    arg                     : UNRESOLVED_ufixed;  -- ufixed point input
    constant size           : NATURAL;            -- length of output
    constant overflow_style : fixed_overflow_style_type := fixed_overflow_style;
    constant round_style    : fixed_round_style_type    := fixed_round_style)
    return UNRESOLVED_UNSIGNED is
  begin
    return to_uns(resize (arg            => arg,
                          left_index     => size-1,
                          right_index    => 0,
                          round_style    => round_style,
                          overflow_style => overflow_style));
  end function to_unsigned;

  function to_unsigned (
    arg                     : UNRESOLVED_ufixed;    -- ufixed point input
    size_res                : UNRESOLVED_UNSIGNED;  -- length of output
    constant overflow_style : fixed_overflow_style_type := fixed_overflow_style;
    constant round_style    : fixed_round_style_type    := fixed_round_style)
    return UNRESOLVED_UNSIGNED is
  begin
    return to_unsigned (arg            => arg,
                        size           => size_res'length,
                        round_style    => round_style,
                        overflow_style => overflow_style);
  end function to_unsigned;

  function to_signed (
    arg                     : UNRESOLVED_sfixed;  -- sfixed point input
    constant size           : NATURAL;            -- length of output
    constant overflow_style : fixed_overflow_style_type := fixed_overflow_style;
    constant round_style    : fixed_round_style_type    := fixed_round_style)
    return UNRESOLVED_SIGNED is
  begin
    return to_s(resize (arg            => arg,
                        left_index     => size-1,
                        right_index    => 0,
                        round_style    => round_style,
                        overflow_style => overflow_style));
  end function to_signed;

  function to_signed (
    arg                     : UNRESOLVED_sfixed;  -- sfixed point input
    size_res                : UNRESOLVED_SIGNED;  -- used for length of output
    constant overflow_style : fixed_overflow_style_type := fixed_overflow_style;
    constant round_style    : fixed_round_style_type    := fixed_round_style)
    return UNRESOLVED_SIGNED is
  begin
    return to_signed (arg            => arg,
                      size           => size_res'length,
                      round_style    => round_style,
                      overflow_style => overflow_style);
  end function to_signed;

  function to_real (
    arg : UNRESOLVED_ufixed)            -- ufixed point input
    return REAL
  is
    constant left_index  : INTEGER := arg'high;
    constant right_index : INTEGER := arg'low;
    variable result      : REAL;        -- result
    variable arg_int     : UNRESOLVED_ufixed (left_index downto right_index);
  begin
    if (arg'length < 1) then
      return 0.0;
    end if;
    arg_int := To_X01(cleanvec(arg));
    if (Is_X(arg_int)) then
      assert no_warning
        report fixed_generic_pkg'instance_name
        & "TO_REAL (ufixed): metavalue detected, returning 0.0"
        severity warning;
      return 0.0;
    end if;
    result := 0.0;
    for i in arg_int'range loop
      if (arg_int(i) = '1') then
        result := result + (2.0**i);
      end if;
    end loop;
    return result;
  end function to_real;

  function to_real (
    arg : UNRESOLVED_sfixed)            -- ufixed point input
    return REAL
  is
    constant left_index  : INTEGER := arg'high;
    constant right_index : INTEGER := arg'low;
    variable result      : REAL;        -- result
    variable arg_int     : UNRESOLVED_sfixed (left_index downto right_index);
    -- unsigned version of argument
    variable arg_uns     : UNRESOLVED_ufixed (left_index downto right_index);
    -- absolute of argument
  begin
    if (arg'length < 1) then
      return 0.0;
    end if;
    arg_int := to_X01(cleanvec(arg));
    if (Is_X(arg_int)) then
      assert no_warning
        report fixed_generic_pkg'instance_name
        & "TO_REAL (sfixed): metavalue detected, returning 0.0"
        severity warning;
      return 0.0;
    end if;
    arg_uns := to_ufixed (arg_int);
    result  := to_real (arg_uns);
    if (arg_int(arg_int'high) = '1') then
      result := -result;
    end if;
    return result;
  end function to_real;

  function to_integer (
    arg                     : UNRESOLVED_ufixed;  -- fixed point input
    constant overflow_style : fixed_overflow_style_type := fixed_overflow_style;
    constant round_style    : fixed_round_style_type    := fixed_round_style)
    return NATURAL
  is
    constant left_index : INTEGER := arg'high;
    variable arg_uns    : UNRESOLVED_UNSIGNED (left_index+1 downto 0)
      := (others => '0');
  begin
    if (arg'length < 1) then
      return 0;
    end if;
    if (Is_X (arg)) then
      assert no_warning
        report fixed_generic_pkg'instance_name
        & "TO_INTEGER (ufixed): metavalue detected, returning 0"
        severity warning;
      return 0;
    end if;
    if (left_index < -1) then
      return 0;
    end if;
    arg_uns := to_uns(resize (arg            => arg,
                              left_index     => arg_uns'high,
                              right_index    => 0,
                              round_style    => round_style,
                              overflow_style => overflow_style));
    return to_integer (arg_uns);
  end function to_integer;

  function to_integer (
    arg                     : UNRESOLVED_sfixed;  -- fixed point input
    constant overflow_style : fixed_overflow_style_type := fixed_overflow_style;
    constant round_style    : fixed_round_style_type    := fixed_round_style)
    return INTEGER
  is
    constant left_index  : INTEGER := arg'high;
    variable arg_s       : UNRESOLVED_SIGNED (left_index+1 downto 0);
  begin
    if (arg'length < 1) then
      return 0;
    end if;
    if (Is_X (arg)) then
      assert no_warning
        report fixed_generic_pkg'instance_name
        & "TO_INTEGER (sfixed): metavalue detected, returning 0"
        severity warning;
      return 0;
    end if;
    if (left_index < -1) then
      return 0;
    end if;
    arg_s := to_s(resize (arg            => arg,
                          left_index     => arg_s'high,
                          right_index    => 0,
                          round_style    => round_style,
                          overflow_style => overflow_style));
    return to_integer (arg_s);
  end function to_integer;

  function to_01 (
    s             : UNRESOLVED_ufixed;              -- ufixed point input
    constant XMAP : STD_ULOGIC := '0')              -- Map x to
    return UNRESOLVED_ufixed
  is
  begin
    if (s'length < 1) then
      assert no_warning
        report fixed_generic_pkg'instance_name
        & "TO_01(ufixed): null detected, returning NULL"
        severity warning;
      return NAUF;
    end if;
    return to_fixed (to_01(to_uns(s), XMAP), s'high, s'low);
  end function to_01;

  function to_01 (
    s             : UNRESOLVED_sfixed;  -- sfixed point input
    constant XMAP : STD_ULOGIC := '0')  -- Map x to
    return UNRESOLVED_sfixed
  is
  begin
    if (s'length < 1) then
      assert no_warning
        report fixed_generic_pkg'instance_name
        & "TO_01(sfixed): null detected, returning NULL"
        severity warning;
      return NASF;
    end if;
    return to_fixed (to_01(to_s(s), XMAP), s'high, s'low);
  end function to_01;

  function Is_X (
    arg : UNRESOLVED_ufixed)
    return BOOLEAN
  is
    variable argslv : STD_ULOGIC_VECTOR (arg'length-1 downto 0);  -- slv
  begin
    argslv := to_sulv(arg);
    return Is_X (argslv);
  end function Is_X;

  function Is_X (
    arg : UNRESOLVED_sfixed)
    return BOOLEAN
  is
    variable argslv : STD_ULOGIC_VECTOR (arg'length-1 downto 0);  -- slv
  begin
    argslv := to_sulv(arg);
    return Is_X (argslv);
  end function Is_X;

  function To_X01 (
    arg : UNRESOLVED_ufixed)
    return UNRESOLVED_ufixed is
  begin
    return to_ufixed (To_X01(to_sulv(arg)), arg'high, arg'low);
  end function To_X01;

  function to_X01 (
    arg : UNRESOLVED_sfixed)
    return UNRESOLVED_sfixed is
  begin
    return to_sfixed (To_X01(to_sulv(arg)), arg'high, arg'low);
  end function to_X01;

  function To_X01Z (
    arg : UNRESOLVED_ufixed)
    return UNRESOLVED_ufixed is
  begin
    return to_ufixed (To_X01Z(to_sulv(arg)), arg'high, arg'low);
  end function To_X01Z;

  function to_X01Z (
    arg : UNRESOLVED_sfixed)
    return UNRESOLVED_sfixed is
  begin
    return to_sfixed (To_X01Z(to_sulv(arg)), arg'high, arg'low);
  end function to_X01Z;

  function To_UX01 (
    arg : UNRESOLVED_ufixed)
    return UNRESOLVED_ufixed is
  begin
    return to_ufixed (To_UX01(to_sulv(arg)), arg'high, arg'low);
  end function To_UX01;

  function to_UX01 (
    arg : UNRESOLVED_sfixed)
    return UNRESOLVED_sfixed is
  begin
    return to_sfixed (To_UX01(to_sulv(arg)), arg'high, arg'low);
  end function to_UX01;

  function resize (
    arg                     : UNRESOLVED_ufixed;            -- input
    constant left_index     : INTEGER;  -- integer portion
    constant right_index    : INTEGER;  -- size of fraction
    constant overflow_style : fixed_overflow_style_type := fixed_overflow_style;
    constant round_style    : fixed_round_style_type    := fixed_round_style)
    return UNRESOLVED_ufixed
  is
    constant arghigh : INTEGER := maximum (arg'high, arg'low);
    constant arglow  : INTEGER := mine (arg'high, arg'low);
    variable invec   : UNRESOLVED_ufixed (arghigh downto arglow);
    variable result  : UNRESOLVED_ufixed(left_index downto right_index) :=
      (others => '0');
    variable needs_rounding : BOOLEAN := false;
  begin  -- resize
    if (arg'length < 1) or (result'length < 1) then
      return NAUF;
    elsif (invec'length < 1) then
      return result;                    -- string literal value
    else
      invec := cleanvec(arg);
      if (right_index > arghigh) then   -- return top zeros
        needs_rounding := (round_style = fixed_round) and
                          (right_index = arghigh+1);
      elsif (left_index < arglow) then  -- return overflow
        if (overflow_style = fixed_saturate) and
          (or(to_sulv(invec)) = '1') then
          result := saturate (result'high, result'low);     -- saturate
        end if;
      elsif (arghigh > left_index) then
        -- wrap or saturate?
        if (overflow_style = fixed_saturate and
            or (to_sulv(invec(arghigh downto left_index+1))) = '1')
        then
          result := saturate (result'high, result'low);     -- saturate
        else
          if (arglow >= right_index) then
            result (left_index downto arglow) :=
              invec(left_index downto arglow);
          else
            result (left_index downto right_index) :=
              invec (left_index downto right_index);
            needs_rounding := (round_style = fixed_round);  -- round
          end if;
        end if;
      else                              -- arghigh <= integer width
        if (arglow >= right_index) then
          result (arghigh downto arglow) := invec;
        else
          result (arghigh downto right_index) :=
            invec (arghigh downto right_index);
          needs_rounding := (round_style = fixed_round);    -- round
        end if;
      end if;
      -- Round result
      if needs_rounding then
        result := round_fixed (arg            => result,
                               remainder      => invec (right_index-1
                                                        downto arglow),
                               overflow_style => overflow_style);
      end if;
      return result;
    end if;
  end function resize;

  function resize (
    arg                     : UNRESOLVED_sfixed;          -- input
    constant left_index     : INTEGER;  -- integer portion
    constant right_index    : INTEGER;  -- size of fraction
    constant overflow_style : fixed_overflow_style_type := fixed_overflow_style;
    constant round_style    : fixed_round_style_type    := fixed_round_style)
    return UNRESOLVED_sfixed
  is
    constant arghigh : INTEGER := maximum (arg'high, arg'low);
    constant arglow  : INTEGER := mine (arg'high, arg'low);
    variable invec   : UNRESOLVED_sfixed (arghigh downto arglow);
    variable result  : UNRESOLVED_sfixed(left_index downto right_index) :=
      (others => '0');
    variable reduced        : STD_ULOGIC;
    variable needs_rounding : BOOLEAN := false;           -- rounding
  begin  -- resize
    if (arg'length < 1) or (result'length < 1) then
      return NASF;
    elsif (invec'length < 1) then
      return result;                    -- string literal value
    else
      invec := cleanvec(arg);
      if (right_index > arghigh) then   -- return top zeros
        if (arg'low /= INTEGER'low) then  -- check for a literal
          result := (others => arg(arghigh));             -- sign extend
        end if;
        needs_rounding := (round_style = fixed_round) and
                          (right_index = arghigh+1);
      elsif (left_index < arglow) then  -- return overflow
        if (overflow_style = fixed_saturate) then
          reduced := or (to_sulv(invec));
          if (reduced = '1') then
            if (invec(arghigh) = '0') then
              -- saturate POSITIVE
              result := saturate (result'high, result'low);
            else
              -- saturate negative
              result := not saturate (result'high, result'low);
            end if;
            -- else return 0 (input was 0)
          end if;
          -- else return 0 (wrap)
        end if;
      elsif (arghigh > left_index) then
        if (invec(arghigh) = '0') then
          reduced := or (to_sulv(invec(arghigh-1 downto
                                      left_index)));
          if overflow_style = fixed_saturate and reduced = '1' then
            -- saturate positive
            result := saturate (result'high, result'low);
          else
            if (right_index > arglow) then
              result         := invec (left_index downto right_index);
              needs_rounding := (round_style = fixed_round);
            else
              result (left_index downto arglow) :=
                invec (left_index downto arglow);
            end if;
          end if;
        else
          reduced := and (to_sulv(invec(arghigh-1 downto
                                       left_index)));
          if overflow_style = fixed_saturate and reduced = '0' then
            result := not saturate (result'high, result'low);
          else
            if (right_index > arglow) then
              result         := invec (left_index downto right_index);
              needs_rounding := (round_style = fixed_round);
            else
              result (left_index downto arglow) :=
                invec (left_index downto arglow);
            end if;
          end if;
        end if;
      else                              -- arghigh <= integer width
        if (arglow >= right_index) then
          result (arghigh downto arglow) := invec;
        else
          result (arghigh downto right_index) :=
            invec (arghigh downto right_index);
          needs_rounding := (round_style = fixed_round);  -- round
        end if;
        if (left_index > arghigh) then  -- sign extend
          result(left_index downto arghigh+1) := (others => invec(arghigh));
        end if;
      end if;
      -- Round result
      if (needs_rounding) then
        result := round_fixed (arg            => result,
                               remainder      => invec (right_index-1
                                                        downto arglow),
                               overflow_style => overflow_style);
      end if;
      return result;
    end if;
  end function resize;

  -- size_res functions
  -- These functions compute the size from a passed variable named "size_res"
  -- The only part of this variable used it it's size, it is never passed
  -- to a lower level routine.
  function to_ufixed (
    arg      : STD_ULOGIC_VECTOR;       -- shifted vector
    size_res : UNRESOLVED_ufixed)       -- for size only
    return UNRESOLVED_ufixed
  is
    constant fw     : INTEGER := mine (size_res'low, size_res'low);  -- catch literals
    variable result : UNRESOLVED_ufixed (size_res'left downto fw);
  begin
    if (result'length < 1 or arg'length < 1) then
      return NAUF;
    else
      result := to_ufixed (arg         => arg,
                           left_index  => size_res'high,
                           right_index => size_res'low);
      return result;
    end if;
  end function to_ufixed;

  function to_sfixed (
    arg      : STD_ULOGIC_VECTOR;       -- shifted vector
    size_res : UNRESOLVED_sfixed)       -- for size only
    return UNRESOLVED_sfixed
  is
    constant fw     : INTEGER := mine (size_res'low, size_res'low);  -- catch literals
    variable result : UNRESOLVED_sfixed (size_res'left downto fw);
  begin
    if (result'length < 1 or arg'length < 1) then
      return NASF;
    else
      result := to_sfixed (arg         => arg,
                           left_index  => size_res'high,
                           right_index => size_res'low);
      return result;
    end if;
  end function to_sfixed;

  function to_ufixed (
    arg                     : NATURAL;            -- integer
    size_res                : UNRESOLVED_ufixed;  -- for size only
    constant overflow_style : fixed_overflow_style_type := fixed_overflow_style;
    constant round_style    : fixed_round_style_type    := fixed_round_style)
    return UNRESOLVED_ufixed
  is
    constant fw     : INTEGER := mine (size_res'low, size_res'low);  -- catch literals
    variable result : UNRESOLVED_ufixed (size_res'left downto fw);
  begin
    if (result'length < 1) then
      return NAUF;
    else
      result := to_ufixed (arg            => arg,
                           left_index     => size_res'high,
                           right_index    => size_res'low,
                           round_style    => round_style,
                           overflow_style => overflow_style);
      return result;
    end if;
  end function to_ufixed;

  function to_sfixed (
    arg                     : INTEGER;            -- integer
    size_res                : UNRESOLVED_sfixed;  -- for size only
    constant overflow_style : fixed_overflow_style_type := fixed_overflow_style;
    constant round_style    : fixed_round_style_type    := fixed_round_style)
    return UNRESOLVED_sfixed
  is
    constant fw     : INTEGER := mine (size_res'low, size_res'low);  -- catch literals
    variable result : UNRESOLVED_sfixed (size_res'left downto fw);
  begin
    if (result'length < 1) then
      return NASF;
    else
      result := to_sfixed (arg            => arg,
                           left_index     => size_res'high,
                           right_index    => size_res'low,
                           round_style    => round_style,
                           overflow_style => overflow_style);
      return result;
    end if;
  end function to_sfixed;

  function to_ufixed (
    arg                     : REAL;     -- real
    size_res                : UNRESOLVED_ufixed;  -- for size only
    constant overflow_style : fixed_overflow_style_type := fixed_overflow_style;
    constant round_style    : fixed_round_style_type    := fixed_round_style;
    constant guard_bits     : NATURAL                   := fixed_guard_bits)  -- # of guard bits
    return UNRESOLVED_ufixed
  is
    constant fw     : INTEGER := mine (size_res'low, size_res'low);  -- catch literals
    variable result : UNRESOLVED_ufixed (size_res'left downto fw);
  begin
    if (result'length < 1) then
      return NAUF;
    else
      result := to_ufixed (arg            => arg,
                           left_index     => size_res'high,
                           right_index    => size_res'low,
                           guard_bits     => guard_bits,
                           round_style    => round_style,
                           overflow_style => overflow_style);
      return result;
    end if;
  end function to_ufixed;

  function to_sfixed (
    arg                     : REAL;     -- real
    size_res                : UNRESOLVED_sfixed;  -- for size only
    constant overflow_style : fixed_overflow_style_type := fixed_overflow_style;
    constant round_style    : fixed_round_style_type    := fixed_round_style;
    constant guard_bits     : NATURAL                   := fixed_guard_bits)  -- # of guard bits
    return UNRESOLVED_sfixed
  is
    constant fw     : INTEGER := mine (size_res'low, size_res'low);  -- catch literals
    variable result : UNRESOLVED_sfixed (size_res'left downto fw);
  begin
    if (result'length < 1) then
      return NASF;
    else
      result := to_sfixed (arg            => arg,
                           left_index     => size_res'high,
                           right_index    => size_res'low,
                           guard_bits     => guard_bits,
                           round_style    => round_style,
                           overflow_style => overflow_style);
      return result;
    end if;
  end function to_sfixed;

  function to_ufixed (
    arg                     : UNRESOLVED_UNSIGNED;  -- unsigned
    size_res                : UNRESOLVED_ufixed;    -- for size only
    constant overflow_style : fixed_overflow_style_type := fixed_overflow_style;
    constant round_style    : fixed_round_style_type    := fixed_round_style)
    return UNRESOLVED_ufixed
  is
    constant fw     : INTEGER := mine (size_res'low, size_res'low);  -- catch literals
    variable result : UNRESOLVED_ufixed (size_res'left downto fw);
  begin
    if (result'length < 1 or arg'length < 1) then
      return NAUF;
    else
      result := to_ufixed (arg            => arg,
                           left_index     => size_res'high,
                           right_index    => size_res'low,
                           round_style    => round_style,
                           overflow_style => overflow_style);
      return result;
    end if;
  end function to_ufixed;

  function to_sfixed (
    arg                     : UNRESOLVED_SIGNED;  -- signed
    size_res                : UNRESOLVED_sfixed;  -- for size only
    constant overflow_style : fixed_overflow_style_type := fixed_overflow_style;
    constant round_style    : fixed_round_style_type    := fixed_round_style)
    return UNRESOLVED_sfixed
  is
    constant fw     : INTEGER := mine (size_res'low, size_res'low);  -- catch literals
    variable result : UNRESOLVED_sfixed (size_res'left downto fw);
  begin
    if (result'length < 1 or arg'length < 1) then
      return NASF;
    else
      result := to_sfixed (arg            => arg,
                           left_index     => size_res'high,
                           right_index    => size_res'low,
                           round_style    => round_style,
                           overflow_style => overflow_style);
      return result;
    end if;
  end function to_sfixed;

  function resize (
    arg                     : UNRESOLVED_ufixed;  -- input
    size_res                : UNRESOLVED_ufixed;  -- for size only
    constant overflow_style : fixed_overflow_style_type := fixed_overflow_style;
    constant round_style    : fixed_round_style_type    := fixed_round_style)
    return UNRESOLVED_ufixed
  is
    constant fw     : INTEGER := mine (size_res'low, size_res'low);  -- catch literals
    variable result : UNRESOLVED_ufixed (size_res'high downto fw);
  begin
    if (result'length < 1 or arg'length < 1) then
      return NAUF;
    else
      result := resize (arg            => arg,
                        left_index     => size_res'high,
                        right_index    => size_res'low,
                        round_style    => round_style,
                        overflow_style => overflow_style);
      return result;
    end if;
  end function resize;

  function resize (
    arg                     : UNRESOLVED_sfixed;  -- input
    size_res                : UNRESOLVED_sfixed;  -- for size only
    constant overflow_style : fixed_overflow_style_type := fixed_overflow_style;
    constant round_style    : fixed_round_style_type    := fixed_round_style)
    return UNRESOLVED_sfixed
  is
    constant fw     : INTEGER := mine (size_res'low, size_res'low);  -- catch literals
    variable result : UNRESOLVED_sfixed (size_res'high downto fw);
  begin
    if (result'length < 1 or arg'length < 1) then
      return NASF;
    else
      result := resize (arg            => arg,
                        left_index     => size_res'high,
                        right_index    => size_res'low,
                        round_style    => round_style,
                        overflow_style => overflow_style);
      return result;
    end if;
  end function resize;

  -- Overloaded math functions for real
  function "+" (
    l : UNRESOLVED_ufixed;              -- fixed point input
    r : REAL)
    return UNRESOLVED_ufixed is
  begin
    return (l + to_ufixed (r, l'high, l'low));
  end function "+";

  function "+" (
    l : REAL;
    r : UNRESOLVED_ufixed)              -- fixed point input
    return UNRESOLVED_ufixed is
  begin
    return (to_ufixed (l, r'high, r'low) + r);
  end function "+";

  function "+" (
    l : UNRESOLVED_sfixed;              -- fixed point input
    r : REAL)
    return UNRESOLVED_sfixed is
  begin
    return (l + to_sfixed (r, l'high, l'low));
  end function "+";

  function "+" (
    l : REAL;
    r : UNRESOLVED_sfixed)              -- fixed point input
    return UNRESOLVED_sfixed is
  begin
    return (to_sfixed (l, r'high, r'low) + r);
  end function "+";

  function "-" (
    l : UNRESOLVED_ufixed;              -- fixed point input
    r : REAL)
    return UNRESOLVED_ufixed is
  begin
    return (l - to_ufixed (r, l'high, l'low));
  end function "-";

  function "-" (
    l : REAL;
    r : UNRESOLVED_ufixed)              -- fixed point input
    return UNRESOLVED_ufixed is
  begin
    return (to_ufixed (l, r'high, r'low) - r);
  end function "-";

  function "-" (
    l : UNRESOLVED_sfixed;              -- fixed point input
    r : REAL)
    return UNRESOLVED_sfixed is
  begin
    return (l - to_sfixed (r, l'high, l'low));
  end function "-";

  function "-" (
    l : REAL;
    r : UNRESOLVED_sfixed)              -- fixed point input
    return UNRESOLVED_sfixed is
  begin
    return (to_sfixed (l, r'high, r'low) - r);
  end function "-";

  function "*" (
    l : UNRESOLVED_ufixed;              -- fixed point input
    r : REAL)
    return UNRESOLVED_ufixed is
  begin
    return (l * to_ufixed (r, l'high, l'low));
  end function "*";

  function "*" (
    l : REAL;
    r : UNRESOLVED_ufixed)              -- fixed point input
    return UNRESOLVED_ufixed is
  begin
    return (to_ufixed (l, r'high, r'low) * r);
  end function "*";

  function "*" (
    l : UNRESOLVED_sfixed;              -- fixed point input
    r : REAL)
    return UNRESOLVED_sfixed is
  begin
    return (l * to_sfixed (r, l'high, l'low));
  end function "*";

  function "*" (
    l : REAL;
    r : UNRESOLVED_sfixed)              -- fixed point input
    return UNRESOLVED_sfixed is
  begin
    return (to_sfixed (l, r'high, r'low) * r);
  end function "*";

  function "/" (
    l : UNRESOLVED_ufixed;              -- fixed point input
    r : REAL)
    return UNRESOLVED_ufixed is
  begin
    return (l / to_ufixed (r, l'high, l'low));
  end function "/";

  function "/" (
    l : REAL;
    r : UNRESOLVED_ufixed)              -- fixed point input
    return UNRESOLVED_ufixed is
  begin
    return (to_ufixed (l, r'high, r'low) / r);
  end function "/";

  function "/" (
    l : UNRESOLVED_sfixed;              -- fixed point input
    r : REAL)
    return UNRESOLVED_sfixed is
  begin
    return (l / to_sfixed (r, l'high, l'low));
  end function "/";

  function "/" (
    l : REAL;
    r : UNRESOLVED_sfixed)              -- fixed point input
    return UNRESOLVED_sfixed is
  begin
    return (to_sfixed (l, r'high, r'low) / r);
  end function "/";

  function "rem" (
    l : UNRESOLVED_ufixed;              -- fixed point input
    r : REAL)
    return UNRESOLVED_ufixed is
  begin
    return (l rem to_ufixed (r, l'high, l'low));
  end function "rem";

  function "rem" (
    l : REAL;
    r : UNRESOLVED_ufixed)              -- fixed point input
    return UNRESOLVED_ufixed is
  begin
    return (to_ufixed (l, r'high, r'low) rem r);
  end function "rem";

  function "rem" (
    l : UNRESOLVED_sfixed;              -- fixed point input
    r : REAL)
    return UNRESOLVED_sfixed is
  begin
    return (l rem to_sfixed (r, l'high, l'low));
  end function "rem";

  function "rem" (
    l : REAL;
    r : UNRESOLVED_sfixed)              -- fixed point input
    return UNRESOLVED_sfixed is
  begin
    return (to_sfixed (l, r'high, r'low) rem r);
  end function "rem";

  function "mod" (
    l : UNRESOLVED_ufixed;              -- fixed point input
    r : REAL)
    return UNRESOLVED_ufixed is
  begin
    return (l mod to_ufixed (r, l'high, l'low));
  end function "mod";

  function "mod" (
    l : REAL;
    r : UNRESOLVED_ufixed)              -- fixed point input
    return UNRESOLVED_ufixed is
  begin
    return (to_ufixed (l, r'high, r'low) mod r);
  end function "mod";

  function "mod" (
    l : UNRESOLVED_sfixed;              -- fixed point input
    r : REAL)
    return UNRESOLVED_sfixed is
  begin
    return (l mod to_sfixed (r, l'high, l'low));
  end function "mod";

  function "mod" (
    l : REAL;
    r : UNRESOLVED_sfixed)              -- fixed point input
    return UNRESOLVED_sfixed is
  begin
    return (to_sfixed (l, r'high, r'low) mod r);
  end function "mod";

  -- Overloaded math functions for integers
  function "+" (
    l : UNRESOLVED_ufixed;              -- fixed point input
    r : NATURAL)
    return UNRESOLVED_ufixed is
  begin
    return (l + to_ufixed (r, l'high, 0));
  end function "+";

  function "+" (
    l : NATURAL;
    r : UNRESOLVED_ufixed)              -- fixed point input
    return UNRESOLVED_ufixed is
  begin
    return (to_ufixed (l, r'high, 0) + r);
  end function "+";

  function "+" (
    l : UNRESOLVED_sfixed;              -- fixed point input
    r : INTEGER)
    return UNRESOLVED_sfixed is
  begin
    return (l + to_sfixed (r, l'high, 0));
  end function "+";

  function "+" (
    l : INTEGER;
    r : UNRESOLVED_sfixed)              -- fixed point input
    return UNRESOLVED_sfixed is
  begin
    return (to_sfixed (l, r'high, 0) + r);
  end function "+";

  -- Overloaded functions
  function "-" (
    l : UNRESOLVED_ufixed;              -- fixed point input
    r : NATURAL)
    return UNRESOLVED_ufixed is
  begin
    return (l - to_ufixed (r, l'high, 0));
  end function "-";

  function "-" (
    l : NATURAL;
    r : UNRESOLVED_ufixed)              -- fixed point input
    return UNRESOLVED_ufixed is
  begin
    return (to_ufixed (l, r'high, 0) - r);
  end function "-";

  function "-" (
    l : UNRESOLVED_sfixed;              -- fixed point input
    r : INTEGER)
    return UNRESOLVED_sfixed is
  begin
    return (l - to_sfixed (r, l'high, 0));
  end function "-";

  function "-" (
    l : INTEGER;
    r : UNRESOLVED_sfixed)              -- fixed point input
    return UNRESOLVED_sfixed is
  begin
    return (to_sfixed (l, r'high, 0) - r);
  end function "-";

  -- Overloaded functions
  function "*" (
    l : UNRESOLVED_ufixed;              -- fixed point input
    r : NATURAL)
    return UNRESOLVED_ufixed is
  begin
    return (l * to_ufixed (r, l'high, 0));
  end function "*";

  function "*" (
    l : NATURAL;
    r : UNRESOLVED_ufixed)              -- fixed point input
    return UNRESOLVED_ufixed is
  begin
    return (to_ufixed (l, r'high, 0) * r);
  end function "*";

  function "*" (
    l : UNRESOLVED_sfixed;              -- fixed point input
    r : INTEGER)
    return UNRESOLVED_sfixed is
  begin
    return (l * to_sfixed (r, l'high, 0));
  end function "*";

  function "*" (
    l : INTEGER;
    r : UNRESOLVED_sfixed)              -- fixed point input
    return UNRESOLVED_sfixed is
  begin
    return (to_sfixed (l, r'high, 0) * r);
  end function "*";

  -- Overloaded functions
  function "/" (
    l : UNRESOLVED_ufixed;              -- fixed point input
    r : NATURAL)
    return UNRESOLVED_ufixed is
  begin
    return (l / to_ufixed (r, l'high, 0));
  end function "/";

  function "/" (
    l : NATURAL;
    r : UNRESOLVED_ufixed)              -- fixed point input
    return UNRESOLVED_ufixed is
  begin
    return (to_ufixed (l, r'high, 0) / r);
  end function "/";

  function "/" (
    l : UNRESOLVED_sfixed;              -- fixed point input
    r : INTEGER)
    return UNRESOLVED_sfixed is
  begin
    return (l / to_sfixed (r, l'high, 0));
  end function "/";

  function "/" (
    l : INTEGER;
    r : UNRESOLVED_sfixed)              -- fixed point input
    return UNRESOLVED_sfixed is
  begin
    return (to_sfixed (l, r'high, 0) / r);
  end function "/";

  function "rem" (
    l : UNRESOLVED_ufixed;              -- fixed point input
    r : NATURAL)
    return UNRESOLVED_ufixed is
  begin
    return (l rem to_ufixed (r, l'high, 0));
  end function "rem";

  function "rem" (
    l : NATURAL;
    r : UNRESOLVED_ufixed)              -- fixed point input
    return UNRESOLVED_ufixed is
  begin
    return (to_ufixed (l, r'high, 0) rem r);
  end function "rem";

  function "rem" (
    l : UNRESOLVED_sfixed;              -- fixed point input
    r : INTEGER)
    return UNRESOLVED_sfixed is
  begin
    return (l rem to_sfixed (r, l'high, 0));
  end function "rem";

  function "rem" (
    l : INTEGER;
    r : UNRESOLVED_sfixed)              -- fixed point input
    return UNRESOLVED_sfixed is
  begin
    return (to_sfixed (l, r'high, 0) rem r);
  end function "rem";

  function "mod" (
    l : UNRESOLVED_ufixed;              -- fixed point input
    r : NATURAL)
    return UNRESOLVED_ufixed is
  begin
    return (l mod to_ufixed (r, l'high, 0));
  end function "mod";

  function "mod" (
    l : NATURAL;
    r : UNRESOLVED_ufixed)              -- fixed point input
    return UNRESOLVED_ufixed is
  begin
    return (to_ufixed (l, r'high, 0) mod r);
  end function "mod";

  function "mod" (
    l : UNRESOLVED_sfixed;              -- fixed point input
    r : INTEGER)
    return UNRESOLVED_sfixed is
  begin
    return (l mod to_sfixed (r, l'high, 0));
  end function "mod";

  function "mod" (
    l : INTEGER;
    r : UNRESOLVED_sfixed)              -- fixed point input
    return UNRESOLVED_sfixed is
  begin
    return (to_sfixed (l, r'high, 0) mod r);
  end function "mod";

  -- overloaded ufixed compare functions with integer
  function "=" (
    l : UNRESOLVED_ufixed;
    r : NATURAL)                        -- fixed point input
    return BOOLEAN is
  begin
    return (l = to_ufixed (r, l'high, l'low));
  end function "=";

  function "/=" (
    l : UNRESOLVED_ufixed;
    r : NATURAL)                        -- fixed point input
    return BOOLEAN is
  begin
    return (l /= to_ufixed (r, l'high, l'low));
  end function "/=";

  function ">=" (
    l : UNRESOLVED_ufixed;
    r : NATURAL)                        -- fixed point input
    return BOOLEAN is
  begin
    return (l >= to_ufixed (r, l'high, l'low));
  end function ">=";

  function "<=" (
    l : UNRESOLVED_ufixed;
    r : NATURAL)                        -- fixed point input
    return BOOLEAN is
  begin
    return (l <= to_ufixed (r, l'high, l'low));
  end function "<=";

  function ">" (
    l : UNRESOLVED_ufixed;
    r : NATURAL)                        -- fixed point input
    return BOOLEAN is
  begin
    return (l > to_ufixed (r, l'high, l'low));
  end function ">";

  function "<" (
    l : UNRESOLVED_ufixed;
    r : NATURAL)                        -- fixed point input
    return BOOLEAN is
  begin
    return (l < to_ufixed (r, l'high, l'low));
  end function "<";

  function "?=" (
    l : UNRESOLVED_ufixed;
    r : NATURAL)                        -- fixed point input
    return STD_ULOGIC is
  begin
    return (l ?= to_ufixed (r, l'high, l'low));
  end function "?=";

  function "?/=" (
    l : UNRESOLVED_ufixed;
    r : NATURAL)                        -- fixed point input
    return STD_ULOGIC is
  begin
    return (l ?/= to_ufixed (r, l'high, l'low));
  end function "?/=";

  function "?>=" (
    l : UNRESOLVED_ufixed;
    r : NATURAL)                        -- fixed point input
    return STD_ULOGIC is
  begin
    return (l ?>= to_ufixed (r, l'high, l'low));
  end function "?>=";

  function "?<=" (
    l : UNRESOLVED_ufixed;
    r : NATURAL)                        -- fixed point input
    return STD_ULOGIC is
  begin
    return (l ?<= to_ufixed (r, l'high, l'low));
  end function "?<=";

  function "?>" (
    l : UNRESOLVED_ufixed;
    r : NATURAL)                        -- fixed point input
    return STD_ULOGIC is
  begin
    return (l ?> to_ufixed (r, l'high, l'low));
  end function "?>";

  function "?<" (
    l : UNRESOLVED_ufixed;
    r : NATURAL)                        -- fixed point input
    return STD_ULOGIC is
  begin
    return (l ?< to_ufixed (r, l'high, l'low));
  end function "?<";

  function maximum (
    l : UNRESOLVED_ufixed;              -- fixed point input
    r : NATURAL)
    return UNRESOLVED_ufixed is
  begin
    return maximum (l, to_ufixed (r, l'high, l'low));
  end function maximum;

  function minimum (
    l : UNRESOLVED_ufixed;              -- fixed point input
    r : NATURAL)
    return UNRESOLVED_ufixed is
  begin
    return minimum (l, to_ufixed (r, l'high, l'low));
  end function minimum;

  -- NATURAL to ufixed
  function "=" (
    l : NATURAL;
    r : UNRESOLVED_ufixed)              -- fixed point input
    return BOOLEAN is
  begin
    return (to_ufixed (l, r'high, r'low) = r);
  end function "=";

  function "/=" (
    l : NATURAL;
    r : UNRESOLVED_ufixed)              -- fixed point input
    return BOOLEAN is
  begin
    return (to_ufixed (l, r'high, r'low) /= r);
  end function "/=";

  function ">=" (
    l : NATURAL;
    r : UNRESOLVED_ufixed)              -- fixed point input
    return BOOLEAN is
  begin
    return (to_ufixed (l, r'high, r'low) >= r);
  end function ">=";

  function "<=" (
    l : NATURAL;
    r : UNRESOLVED_ufixed)              -- fixed point input
    return BOOLEAN is
  begin
    return (to_ufixed (l, r'high, r'low) <= r);
  end function "<=";

  function ">" (
    l : NATURAL;
    r : UNRESOLVED_ufixed)              -- fixed point input
    return BOOLEAN is
  begin
    return (to_ufixed (l, r'high, r'low) > r);
  end function ">";

  function "<" (
    l : NATURAL;
    r : UNRESOLVED_ufixed)              -- fixed point input
    return BOOLEAN is
  begin
    return (to_ufixed (l, r'high, r'low) < r);
  end function "<";

  function "?=" (
    l : NATURAL;
    r : UNRESOLVED_ufixed)              -- fixed point input
    return STD_ULOGIC is
  begin
    return (to_ufixed (l, r'high, r'low) ?= r);
  end function "?=";

  function "?/=" (
    l : NATURAL;
    r : UNRESOLVED_ufixed)              -- fixed point input
    return STD_ULOGIC is
  begin
    return (to_ufixed (l, r'high, r'low) ?/= r);
  end function "?/=";

  function "?>=" (
    l : NATURAL;
    r : UNRESOLVED_ufixed)              -- fixed point input
    return STD_ULOGIC is
  begin
    return (to_ufixed (l, r'high, r'low) ?>= r);
  end function "?>=";

  function "?<=" (
    l : NATURAL;
    r : UNRESOLVED_ufixed)              -- fixed point input
    return STD_ULOGIC is
  begin
    return (to_ufixed (l, r'high, r'low) ?<= r);
  end function "?<=";

  function "?>" (
    l : NATURAL;
    r : UNRESOLVED_ufixed)              -- fixed point input
    return STD_ULOGIC is
  begin
    return (to_ufixed (l, r'high, r'low) ?> r);
  end function "?>";

  function "?<" (
    l : NATURAL;
    r : UNRESOLVED_ufixed)              -- fixed point input
    return STD_ULOGIC is
  begin
    return (to_ufixed (l, r'high, r'low) ?< r);
  end function "?<";

  function maximum (
    l : NATURAL;
    r : UNRESOLVED_ufixed)              -- fixed point input
    return UNRESOLVED_ufixed is
  begin
    return maximum (to_ufixed (l, r'high, r'low), r);
  end function maximum;

  function minimum (
    l : NATURAL;
    r : UNRESOLVED_ufixed)              -- fixed point input
    return UNRESOLVED_ufixed is
  begin
    return minimum (to_ufixed (l, r'high, r'low), r);
  end function minimum;

  -- overloaded ufixed compare functions with real
  function "=" (
    l : UNRESOLVED_ufixed;
    r : REAL)
    return BOOLEAN is
  begin
    return (l = to_ufixed (r, l'high, l'low));
  end function "=";

  function "/=" (
    l : UNRESOLVED_ufixed;
    r : REAL)
    return BOOLEAN is
  begin
    return (l /= to_ufixed (r, l'high, l'low));
  end function "/=";

  function ">=" (
    l : UNRESOLVED_ufixed;
    r : REAL)
    return BOOLEAN is
  begin
    return (l >= to_ufixed (r, l'high, l'low));
  end function ">=";

  function "<=" (
    l : UNRESOLVED_ufixed;
    r : REAL)
    return BOOLEAN is
  begin
    return (l <= to_ufixed (r, l'high, l'low));
  end function "<=";

  function ">" (
    l : UNRESOLVED_ufixed;
    r : REAL)
    return BOOLEAN is
  begin
    return (l > to_ufixed (r, l'high, l'low));
  end function ">";

  function "<" (
    l : UNRESOLVED_ufixed;
    r : REAL)
    return BOOLEAN is
  begin
    return (l < to_ufixed (r, l'high, l'low));
  end function "<";

  function "?=" (
    l : UNRESOLVED_ufixed;
    r : REAL)
    return STD_ULOGIC is
  begin
    return (l ?= to_ufixed (r, l'high, l'low));
  end function "?=";

  function "?/=" (
    l : UNRESOLVED_ufixed;
    r : REAL)
    return STD_ULOGIC is
  begin
    return (l ?/= to_ufixed (r, l'high, l'low));
  end function "?/=";

  function "?>=" (
    l : UNRESOLVED_ufixed;
    r : REAL)
    return STD_ULOGIC is
  begin
    return (l ?>= to_ufixed (r, l'high, l'low));
  end function "?>=";

  function "?<=" (
    l : UNRESOLVED_ufixed;
    r : REAL)
    return STD_ULOGIC is
  begin
    return (l ?<= to_ufixed (r, l'high, l'low));
  end function "?<=";

  function "?>" (
    l : UNRESOLVED_ufixed;
    r : REAL)
    return STD_ULOGIC is
  begin
    return (l ?> to_ufixed (r, l'high, l'low));
  end function "?>";

  function "?<" (
    l : UNRESOLVED_ufixed;
    r : REAL)
    return STD_ULOGIC is
  begin
    return (l ?< to_ufixed (r, l'high, l'low));
  end function "?<";

  function maximum (
    l : UNRESOLVED_ufixed;
    r : REAL)
    return UNRESOLVED_ufixed is
  begin
    return maximum (l, to_ufixed (r, l'high, l'low));
  end function maximum;

  function minimum (
    l : UNRESOLVED_ufixed;
    r : REAL)
    return UNRESOLVED_ufixed is
  begin
    return minimum (l, to_ufixed (r, l'high, l'low));
  end function minimum;

  -- real and ufixed
  function "=" (
    l : REAL;
    r : UNRESOLVED_ufixed)              -- fixed point input
    return BOOLEAN is
  begin
    return (to_ufixed (l, r'high, r'low) = r);
  end function "=";

  function "/=" (
    l : REAL;
    r : UNRESOLVED_ufixed)              -- fixed point input
    return BOOLEAN is
  begin
    return (to_ufixed (l, r'high, r'low) /= r);
  end function "/=";

  function ">=" (
    l : REAL;
    r : UNRESOLVED_ufixed)              -- fixed point input
    return BOOLEAN is
  begin
    return (to_ufixed (l, r'high, r'low) >= r);
  end function ">=";

  function "<=" (
    l : REAL;
    r : UNRESOLVED_ufixed)              -- fixed point input
    return BOOLEAN is
  begin
    return (to_ufixed (l, r'high, r'low) <= r);
  end function "<=";

  function ">" (
    l : REAL;
    r : UNRESOLVED_ufixed)              -- fixed point input
    return BOOLEAN is
  begin
    return (to_ufixed (l, r'high, r'low) > r);
  end function ">";

  function "<" (
    l : REAL;
    r : UNRESOLVED_ufixed)              -- fixed point input
    return BOOLEAN is
  begin
    return (to_ufixed (l, r'high, r'low) < r);
  end function "<";

  function "?=" (
    l : REAL;
    r : UNRESOLVED_ufixed)              -- fixed point input
    return STD_ULOGIC is
  begin
    return (to_ufixed (l, r'high, r'low) ?= r);
  end function "?=";

  function "?/=" (
    l : REAL;
    r : UNRESOLVED_ufixed)              -- fixed point input
    return STD_ULOGIC is
  begin
    return (to_ufixed (l, r'high, r'low) ?/= r);
  end function "?/=";

  function "?>=" (
    l : REAL;
    r : UNRESOLVED_ufixed)              -- fixed point input
    return STD_ULOGIC is
  begin
    return (to_ufixed (l, r'high, r'low) ?>= r);
  end function "?>=";

  function "?<=" (
    l : REAL;
    r : UNRESOLVED_ufixed)              -- fixed point input
    return STD_ULOGIC is
  begin
    return (to_ufixed (l, r'high, r'low) ?<= r);
  end function "?<=";

  function "?>" (
    l : REAL;
    r : UNRESOLVED_ufixed)              -- fixed point input
    return STD_ULOGIC is
  begin
    return (to_ufixed (l, r'high, r'low) ?> r);
  end function "?>";

  function "?<" (
    l : REAL;
    r : UNRESOLVED_ufixed)              -- fixed point input
    return STD_ULOGIC is
  begin
    return (to_ufixed (l, r'high, r'low) ?< r);
  end function "?<";

  function maximum (
    l : REAL;
    r : UNRESOLVED_ufixed)              -- fixed point input
    return UNRESOLVED_ufixed is
  begin
    return maximum (to_ufixed (l, r'high, r'low), r);
  end function maximum;

  function minimum (
    l : REAL;
    r : UNRESOLVED_ufixed)              -- fixed point input
    return UNRESOLVED_ufixed is
  begin
    return minimum (to_ufixed (l, r'high, r'low), r);
  end function minimum;

  -- overloaded sfixed compare functions with integer
  function "=" (
    l : UNRESOLVED_sfixed;
    r : INTEGER)
    return BOOLEAN is
  begin
    return (l = to_sfixed (r, l'high, l'low));
  end function "=";

  function "/=" (
    l : UNRESOLVED_sfixed;
    r : INTEGER)
    return BOOLEAN is
  begin
    return (l /= to_sfixed (r, l'high, l'low));
  end function "/=";

  function ">=" (
    l : UNRESOLVED_sfixed;
    r : INTEGER)
    return BOOLEAN is
  begin
    return (l >= to_sfixed (r, l'high, l'low));
  end function ">=";

  function "<=" (
    l : UNRESOLVED_sfixed;
    r : INTEGER)
    return BOOLEAN is
  begin
    return (l <= to_sfixed (r, l'high, l'low));
  end function "<=";

  function ">" (
    l : UNRESOLVED_sfixed;
    r : INTEGER)
    return BOOLEAN is
  begin
    return (l > to_sfixed (r, l'high, l'low));
  end function ">";

  function "<" (
    l : UNRESOLVED_sfixed;
    r : INTEGER)
    return BOOLEAN is
  begin
    return (l < to_sfixed (r, l'high, l'low));
  end function "<";

  function "?=" (
    l : UNRESOLVED_sfixed;
    r : INTEGER)
    return STD_ULOGIC is
  begin
    return (l ?= to_sfixed (r, l'high, l'low));
  end function "?=";

  function "?/=" (
    l : UNRESOLVED_sfixed;
    r : INTEGER)
    return STD_ULOGIC is
  begin
    return (l ?/= to_sfixed (r, l'high, l'low));
  end function "?/=";

  function "?>=" (
    l : UNRESOLVED_sfixed;
    r : INTEGER)
    return STD_ULOGIC is
  begin
    return (l ?>= to_sfixed (r, l'high, l'low));
  end function "?>=";

  function "?<=" (
    l : UNRESOLVED_sfixed;
    r : INTEGER)
    return STD_ULOGIC is
  begin
    return (l ?<= to_sfixed (r, l'high, l'low));
  end function "?<=";

  function "?>" (
    l : UNRESOLVED_sfixed;
    r : INTEGER)
    return STD_ULOGIC is
  begin
    return (l ?> to_sfixed (r, l'high, l'low));
  end function "?>";

  function "?<" (
    l : UNRESOLVED_sfixed;
    r : INTEGER)
    return STD_ULOGIC is
  begin
    return (l ?< to_sfixed (r, l'high, l'low));
  end function "?<";

  function maximum (
    l : UNRESOLVED_sfixed;
    r : INTEGER)
    return UNRESOLVED_sfixed is
  begin
    return maximum (l, to_sfixed (r, l'high, l'low));
  end function maximum;

  function minimum (
    l : UNRESOLVED_sfixed;
    r : INTEGER)
    return UNRESOLVED_sfixed is
  begin
    return minimum (l, to_sfixed (r, l'high, l'low));
  end function minimum;

  -- integer and sfixed
  function "=" (
    l : INTEGER;
    r : UNRESOLVED_sfixed)              -- fixed point input
    return BOOLEAN is
  begin
    return (to_sfixed (l, r'high, r'low) = r);
  end function "=";

  function "/=" (
    l : INTEGER;
    r : UNRESOLVED_sfixed)              -- fixed point input
    return BOOLEAN is
  begin
    return (to_sfixed (l, r'high, r'low) /= r);
  end function "/=";

  function ">=" (
    l : INTEGER;
    r : UNRESOLVED_sfixed)              -- fixed point input
    return BOOLEAN is
  begin
    return (to_sfixed (l, r'high, r'low) >= r);
  end function ">=";

  function "<=" (
    l : INTEGER;
    r : UNRESOLVED_sfixed)              -- fixed point input
    return BOOLEAN is
  begin
    return (to_sfixed (l, r'high, r'low) <= r);
  end function "<=";

  function ">" (
    l : INTEGER;
    r : UNRESOLVED_sfixed)              -- fixed point input
    return BOOLEAN is
  begin
    return (to_sfixed (l, r'high, r'low) > r);
  end function ">";

  function "<" (
    l : INTEGER;
    r : UNRESOLVED_sfixed)              -- fixed point input
    return BOOLEAN is
  begin
    return (to_sfixed (l, r'high, r'low) < r);
  end function "<";

  function "?=" (
    l : INTEGER;
    r : UNRESOLVED_sfixed)              -- fixed point input
    return STD_ULOGIC is
  begin
    return (to_sfixed (l, r'high, r'low) ?= r);
  end function "?=";

  function "?/=" (
    l : INTEGER;
    r : UNRESOLVED_sfixed)              -- fixed point input
    return STD_ULOGIC is
  begin
    return (to_sfixed (l, r'high, r'low) ?/= r);
  end function "?/=";

  function "?>=" (
    l : INTEGER;
    r : UNRESOLVED_sfixed)              -- fixed point input
    return STD_ULOGIC is
  begin
    return (to_sfixed (l, r'high, r'low) ?>= r);
  end function "?>=";

  function "?<=" (
    l : INTEGER;
    r : UNRESOLVED_sfixed)              -- fixed point input
    return STD_ULOGIC is
  begin
    return (to_sfixed (l, r'high, r'low) ?<= r);
  end function "?<=";

  function "?>" (
    l : INTEGER;
    r : UNRESOLVED_sfixed)              -- fixed point input
    return STD_ULOGIC is
  begin
    return (to_sfixed (l, r'high, r'low) ?> r);
  end function "?>";

  function "?<" (
    l : INTEGER;
    r : UNRESOLVED_sfixed)              -- fixed point input
    return STD_ULOGIC is
  begin
    return (to_sfixed (l, r'high, r'low) ?< r);
  end function "?<";

  function maximum (
    l : INTEGER;
    r : UNRESOLVED_sfixed)
    return UNRESOLVED_sfixed is
  begin
    return maximum (to_sfixed (l, r'high, r'low), r);
  end function maximum;

  function minimum (
    l : INTEGER;
    r : UNRESOLVED_sfixed)
    return UNRESOLVED_sfixed is
  begin
    return minimum (to_sfixed (l, r'high, r'low), r);
  end function minimum;

  -- overloaded sfixed compare functions with real
  function "=" (
    l : UNRESOLVED_sfixed;
    r : REAL)
    return BOOLEAN is
  begin
    return (l = to_sfixed (r, l'high, l'low));
  end function "=";

  function "/=" (
    l : UNRESOLVED_sfixed;
    r : REAL)
    return BOOLEAN is
  begin
    return (l /= to_sfixed (r, l'high, l'low));
  end function "/=";

  function ">=" (
    l : UNRESOLVED_sfixed;
    r : REAL)
    return BOOLEAN is
  begin
    return (l >= to_sfixed (r, l'high, l'low));
  end function ">=";

  function "<=" (
    l : UNRESOLVED_sfixed;
    r : REAL)
    return BOOLEAN is
  begin
    return (l <= to_sfixed (r, l'high, l'low));
  end function "<=";

  function ">" (
    l : UNRESOLVED_sfixed;
    r : REAL)
    return BOOLEAN is
  begin
    return (l > to_sfixed (r, l'high, l'low));
  end function ">";

  function "<" (
    l : UNRESOLVED_sfixed;
    r : REAL)
    return BOOLEAN is
  begin
    return (l < to_sfixed (r, l'high, l'low));
  end function "<";

  function "?=" (
    l : UNRESOLVED_sfixed;
    r : REAL)
    return STD_ULOGIC is
  begin
    return (l ?= to_sfixed (r, l'high, l'low));
  end function "?=";

  function "?/=" (
    l : UNRESOLVED_sfixed;
    r : REAL)
    return STD_ULOGIC is
  begin
    return (l ?/= to_sfixed (r, l'high, l'low));
  end function "?/=";

  function "?>=" (
    l : UNRESOLVED_sfixed;
    r : REAL)
    return STD_ULOGIC is
  begin
    return (l ?>= to_sfixed (r, l'high, l'low));
  end function "?>=";

  function "?<=" (
    l : UNRESOLVED_sfixed;
    r : REAL)
    return STD_ULOGIC is
  begin
    return (l ?<= to_sfixed (r, l'high, l'low));
  end function "?<=";

  function "?>" (
    l : UNRESOLVED_sfixed;
    r : REAL)
    return STD_ULOGIC is
  begin
    return (l ?> to_sfixed (r, l'high, l'low));
  end function "?>";

  function "?<" (
    l : UNRESOLVED_sfixed;
    r : REAL)
    return STD_ULOGIC is
  begin
    return (l ?< to_sfixed (r, l'high, l'low));
  end function "?<";

  function maximum (
    l : UNRESOLVED_sfixed;
    r : REAL)
    return UNRESOLVED_sfixed is
  begin
    return maximum (l, to_sfixed (r, l'high, l'low));
  end function maximum;

  function minimum (
    l : UNRESOLVED_sfixed;
    r : REAL)
    return UNRESOLVED_sfixed is
  begin
    return minimum (l, to_sfixed (r, l'high, l'low));
  end function minimum;

  -- REAL and sfixed
  function "=" (
    l : REAL;
    r : UNRESOLVED_sfixed)              -- fixed point input
    return BOOLEAN is
  begin
    return (to_sfixed (l, r'high, r'low) = r);
  end function "=";

  function "/=" (
    l : REAL;
    r : UNRESOLVED_sfixed)              -- fixed point input
    return BOOLEAN is
  begin
    return (to_sfixed (l, r'high, r'low) /= r);
  end function "/=";

  function ">=" (
    l : REAL;
    r : UNRESOLVED_sfixed)              -- fixed point input
    return BOOLEAN is
  begin
    return (to_sfixed (l, r'high, r'low) >= r);
  end function ">=";

  function "<=" (
    l : REAL;
    r : UNRESOLVED_sfixed)              -- fixed point input
    return BOOLEAN is
  begin
    return (to_sfixed (l, r'high, r'low) <= r);
  end function "<=";

  function ">" (
    l : REAL;
    r : UNRESOLVED_sfixed)              -- fixed point input
    return BOOLEAN is
  begin
    return (to_sfixed (l, r'high, r'low) > r);
  end function ">";

  function "<" (
    l : REAL;
    r : UNRESOLVED_sfixed)              -- fixed point input
    return BOOLEAN is
  begin
    return (to_sfixed (l, r'high, r'low) < r);
  end function "<";

  function "?=" (
    l : REAL;
    r : UNRESOLVED_sfixed)              -- fixed point input
    return STD_ULOGIC is
  begin
    return (to_sfixed (l, r'high, r'low) ?= r);
  end function "?=";

  function "?/=" (
    l : REAL;
    r : UNRESOLVED_sfixed)              -- fixed point input
    return STD_ULOGIC is
  begin
    return (to_sfixed (l, r'high, r'low) ?/= r);
  end function "?/=";

  function "?>=" (
    l : REAL;
    r : UNRESOLVED_sfixed)              -- fixed point input
    return STD_ULOGIC is
  begin
    return (to_sfixed (l, r'high, r'low) ?>= r);
  end function "?>=";

  function "?<=" (
    l : REAL;
    r : UNRESOLVED_sfixed)              -- fixed point input
    return STD_ULOGIC is
  begin
    return (to_sfixed (l, r'high, r'low) ?<= r);
  end function "?<=";

  function "?>" (
    l : REAL;
    r : UNRESOLVED_sfixed)              -- fixed point input
    return STD_ULOGIC is
  begin
    return (to_sfixed (l, r'high, r'low) ?> r);
  end function "?>";

  function "?<" (
    l : REAL;
    r : UNRESOLVED_sfixed)              -- fixed point input
    return STD_ULOGIC is
  begin
    return (to_sfixed (l, r'high, r'low) ?< r);
  end function "?<";

  function maximum (
    l : REAL;
    r : UNRESOLVED_sfixed)
    return UNRESOLVED_sfixed is
  begin
    return maximum (to_sfixed (l, r'high, r'low), r);
  end function maximum;

  function minimum (
    l : REAL;
    r : UNRESOLVED_sfixed)
    return UNRESOLVED_sfixed is
  begin
    return minimum (to_sfixed (l, r'high, r'low), r);
  end function minimum;

  -- copied from std_logic_textio
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
  constant NUS  : STRING(2 to 1) := (others => ' ');

  -- purpose: Skips white space
  procedure skip_whitespace (
    L : inout LINE) is
    variable c : CHARACTER;
    variable left : positive;
  begin
    while L /= null and L.all'length /= 0 loop
      left := L.all'left;
      c := L.all(left);
      if (c = ' ' or c = NBSP or c = HT) then
        read (L, c);
      else
        exit;
      end if;
    end loop;
  end procedure skip_whitespace;

  -- purpose: writes fixed point into a line
  procedure write (
    L         : inout LINE;               -- input line
    VALUE     : in    UNRESOLVED_ufixed;  -- fixed point input
    JUSTIFIED : in    SIDE  := right;
    FIELD     : in    WIDTH := 0) is
    variable s     : STRING(1 to VALUE'length +1) := (others => ' ');
    variable sindx : INTEGER;
  begin  -- function write   Example: 0011.1100
    sindx := 1;
    for i in VALUE'high downto VALUE'low loop
      if i = -1 then
        s(sindx) := '.';
        sindx    := sindx + 1;
      end if;
      s(sindx) := MVL9_to_char(STD_ULOGIC(VALUE(i)));
      sindx    := sindx + 1;
    end loop;
    write(L, s, JUSTIFIED, FIELD);
  end procedure write;

  -- purpose: writes fixed point into a line
  procedure write (
    L         : inout LINE;               -- input line
    VALUE     : in    UNRESOLVED_sfixed;  -- fixed point input
    JUSTIFIED : in    SIDE  := right;
    FIELD     : in    WIDTH := 0) is
    variable s     : STRING(1 to VALUE'length +1);
    variable sindx : INTEGER;
  begin  -- function write   Example: 0011.1100
    sindx := 1;
    for i in VALUE'high downto VALUE'low loop
      if i = -1 then
        s(sindx) := '.';
        sindx    := sindx + 1;
      end if;
      s(sindx) := MVL9_to_char(STD_ULOGIC(VALUE(i)));
      sindx    := sindx + 1;
    end loop;
    write(L, s, JUSTIFIED, FIELD);
  end procedure write;

  procedure READ(L     : inout LINE;
                 VALUE : out   UNRESOLVED_ufixed) is
    -- Possible data:  00000.0000000
    --                 000000000000
    variable c      : CHARACTER;
    variable readOk : BOOLEAN;
    variable i      : INTEGER;          -- index variable
    variable mv : ufixed (VALUE'range);
    variable lastu  : BOOLEAN := false;       -- last character was an "_"
    variable founddot : BOOLEAN := false;  -- found a "."
  begin  -- READ
    VALUE := (VALUE'range => 'U');
    skip_whitespace (L);
    if VALUE'length > 0 then            -- non Null input string
      read (L, c, readOk);
      i := VALUE'high;
      while i >= VALUE'low loop
        if readOk = false then              -- Bail out if there was a bad read
          report fixed_generic_pkg'instance_name & "READ(ufixed) "
            & "End of string encountered"
            severity error;
          return;
        elsif c = '_' then
          if i = VALUE'high then
            report fixed_generic_pkg'instance_name & "READ(ufixed) "
              & "String begins with an ""_""" severity error;
            return;
          elsif lastu then
            report fixed_generic_pkg'instance_name & "READ(ufixed) "
              & "Two underscores detected in input string ""__"""
              severity error;
            return;
          else
            lastu := true;
          end if;
        elsif c = '.' then                -- binary point
          if founddot then
            report fixed_generic_pkg'instance_name & "READ(ufixed) "
              & "Two binary points found in input string" severity error;
            return;
          elsif i /= -1 then                 -- Seperator in the wrong spot
            report fixed_generic_pkg'instance_name & "READ(ufixed) "
              & "Decimal point does not match number format "
              severity error;
            return;
          end if;
          founddot := true;
          lastu := false;
        elsif c = ' ' or c = NBSP or c = HT then  -- reading done.
          report fixed_generic_pkg'instance_name & "READ(ufixed) "
            & "Short read, Space encounted in input string"
            severity error;
          return;
        elsif char_to_MVL9plus(c) = error then
          report fixed_generic_pkg'instance_name & "READ(ufixed) "
            & "Character '" &
            c & "' read, expected STD_ULOGIC literal."
            severity error;
          return;
        else
          mv(i) := char_to_MVL9(c);
          i := i - 1;
          if i < mv'low then
            VALUE := mv;
            return;
          end if;
          lastu := false;
        end if;
        read(L, c, readOk);
      end loop;
    end if;
  end procedure READ;

  procedure READ(L     : inout LINE;
                 VALUE : out   UNRESOLVED_ufixed;
                 GOOD  : out   BOOLEAN) is
    -- Possible data:  00000.0000000
    --                 000000000000
    variable c      : CHARACTER;
    variable readOk : BOOLEAN;
    variable mv : ufixed (VALUE'range);
    variable i      : INTEGER;          -- index variable
    variable lastu  : BOOLEAN := false;       -- last character was an "_"
    variable founddot : BOOLEAN := false;  -- found a "."
  begin  -- READ
    VALUE := (VALUE'range => 'U');
    skip_whitespace (L);
    if VALUE'length > 0 then
      read (L, c, readOk);
      i := VALUE'high;
      GOOD := false;
      while i >= VALUE'low loop
        if not readOk then     -- Bail out if there was a bad read
          return;
        elsif c = '_' then
          if i = VALUE'high then          -- Begins with an "_"
            return;
          elsif lastu then               -- "__" detected
            return;
          else
            lastu := true;
          end if;
        elsif c = '.' then                -- binary point
          if founddot then
            return;
          elsif i /= -1 then                 -- Seperator in the wrong spot
            return;
          end if;
          founddot := true;
          lastu := false;
        elsif (char_to_MVL9plus(c) = error) then   -- Illegal character/short read
          return;
        else
          mv(i) := char_to_MVL9(c);
          i := i - 1;
          if i < mv'low then             -- reading done
            GOOD := true;
            VALUE := mv;
            return;
          end if;
          lastu := false;
        end if;
        read(L, c, readOk);
      end loop;
    else
      GOOD := true;                   -- read into a null array
    end if;
  end procedure READ;

  procedure READ(L     : inout LINE;
                 VALUE : out   UNRESOLVED_sfixed) is
    variable c      : CHARACTER;
    variable readOk : BOOLEAN;
    variable i      : INTEGER;          -- index variable
    variable mv : sfixed (VALUE'range);
    variable lastu  : BOOLEAN := false;       -- last character was an "_"
    variable founddot : BOOLEAN := false;  -- found a "."
  begin  -- READ
    VALUE := (VALUE'range => 'U');
    skip_whitespace (L);
    if VALUE'length > 0 then            -- non Null input string
      read (L, c, readOk);
      i := VALUE'high;
      while i >= VALUE'low loop
        if readOk = false then              -- Bail out if there was a bad read
          report fixed_generic_pkg'instance_name & "READ(sfixed) "
            & "End of string encountered"
            severity error;
          return;
        elsif c = '_' then
          if i = VALUE'high then
            report fixed_generic_pkg'instance_name & "READ(sfixed) "
              & "String begins with an ""_""" severity error;
            return;
          elsif lastu then
            report fixed_generic_pkg'instance_name & "READ(sfixed) "
              & "Two underscores detected in input string ""__"""
              severity error;
            return;
          else
            lastu := true;
          end if;
        elsif c = '.' then                -- binary point
          if founddot then
            report fixed_generic_pkg'instance_name & "READ(sfixed) "
              & "Two binary points found in input string" severity error;
            return;
          elsif i /= -1 then                 -- Seperator in the wrong spot
            report fixed_generic_pkg'instance_name & "READ(sfixed) "
              & "Decimal point does not match number format "
              severity error;
            return;
          end if;
          founddot := true;
          lastu := false;
        elsif c = ' ' or c = NBSP or c = HT then  -- reading done.
          report fixed_generic_pkg'instance_name & "READ(sfixed) "
            & "Short read, Space encounted in input string"
            severity error;
          return;
        elsif char_to_MVL9plus(c) = error then
          report fixed_generic_pkg'instance_name & "READ(sfixed) "
            & "Character '" &
            c & "' read, expected STD_ULOGIC literal."
            severity error;
          return;
        else
          mv(i) := char_to_MVL9(c);
          i := i - 1;
          if i < mv'low then
            VALUE := mv;
            return;
          end if;
          lastu := false;
        end if;
        read(L, c, readOk);
      end loop;
    end if;
  end procedure READ;

  procedure READ(L     : inout LINE;
                 VALUE : out   UNRESOLVED_sfixed;
                 GOOD  : out   BOOLEAN) is
    variable value_ufixed : UNRESOLVED_ufixed (VALUE'range);
  begin  -- READ
    READ (L => L, VALUE => value_ufixed, GOOD => GOOD);
    VALUE := UNRESOLVED_sfixed (value_ufixed);
  end procedure READ;

  -- octal read and write
  procedure owrite (
    L         : inout LINE;               -- input line
    VALUE     : in    UNRESOLVED_ufixed;  -- fixed point input
    JUSTIFIED : in    SIDE  := right;
    FIELD     : in    WIDTH := 0) is
  begin  -- Example 03.30
    write (L         => L,
           VALUE     => TO_OSTRING (VALUE),
           JUSTIFIED => JUSTIFIED,
           FIELD     => FIELD);
  end procedure owrite;

  procedure owrite (
    L         : inout LINE;               -- input line
    VALUE     : in    UNRESOLVED_sfixed;  -- fixed point input
    JUSTIFIED : in    SIDE  := right;
    FIELD     : in    WIDTH := 0) is
  begin  -- Example 03.30
    write (L         => L,
           VALUE     => TO_OSTRING (VALUE),
           JUSTIFIED => JUSTIFIED,
           FIELD     => FIELD);
  end procedure owrite;

  -- Note that for Octal and Hex read, you can not start with a ".",
  -- the read is for numbers formatted "A.BC".  These routines go to
  -- the nearest bounds, so "F.E" will fit into an sfixed (2 downto -3).
  procedure Char2TriBits (C           :     CHARACTER;
                          RESULT      : out STD_ULOGIC_VECTOR(2 downto 0);
                          GOOD        : out BOOLEAN;
                          ISSUE_ERROR : in  BOOLEAN) is
  begin
    case C is
      when '0' => RESULT := o"0"; GOOD := true;
      when '1' => RESULT := o"1"; GOOD := true;
      when '2' => RESULT := o"2"; GOOD := true;
      when '3' => RESULT := o"3"; GOOD := true;
      when '4' => RESULT := o"4"; GOOD := true;
      when '5' => RESULT := o"5"; GOOD := true;
      when '6' => RESULT := o"6"; GOOD := true;
      when '7' => RESULT := o"7"; GOOD := true;
      when 'Z' => RESULT := "ZZZ"; GOOD := true;
      when 'X' => RESULT := "XXX"; GOOD := true;
      when others =>
        assert not ISSUE_ERROR
          report fixed_generic_pkg'instance_name
          & "OREAD Error: Read a '" & C &
          "', expected an Octal character (0-7)."
          severity error;
        RESULT := "UUU";
        GOOD   := false;
    end case;
  end procedure Char2TriBits;

  -- purpose: Routines common to the OREAD routines
  procedure OREAD_common (
    L                : inout LINE;
    slv              : out   STD_ULOGIC_VECTOR;
    igood            : out   BOOLEAN;
    idex             : out INTEGER;
    constant bpoint : in INTEGER;       -- binary point
    constant message : in    BOOLEAN;
    constant smath   : in    BOOLEAN) is

    -- purpose: error message routine
    procedure errmes (
      constant mess : in STRING) is     -- error message
    begin
      if message then
        if smath then
          report fixed_generic_pkg'instance_name
            & "OREAD(sfixed) "
            & mess
            severity error;
        else
          report fixed_generic_pkg'instance_name
            & "OREAD(ufixed) "
            & mess
            severity error;
        end if;
      end if;
    end procedure errmes;
    variable xgood : BOOLEAN;
    variable nybble : STD_ULOGIC_VECTOR (2 downto 0);        -- 3 bits
    variable c : CHARACTER;
    variable i : INTEGER;
    variable lastu  : BOOLEAN := false;       -- last character was an "_"
    variable founddot : BOOLEAN := false;  -- found a dot.
  begin
    skip_whitespace (L);
    if slv'length > 0 then
      i := slv'high;
      read (L, c, xgood);
      while i > 0 loop
        if xgood = false then
          errmes ("Error: end of string encountered");
          exit;
        elsif c = '_' then
          if i = slv'length then
            errmes ("Error: String begins with an ""_""");
            xgood := false;
            exit;
          elsif lastu then
            errmes ("Error: Two underscores detected in input string ""__""");
            xgood := false;
            exit;
          else
            lastu := true;
          end if;
        elsif (c = '.') then
          if (i + 1 /= bpoint) then
            errmes ("encountered ""."" at wrong index");
            xgood := false;
            exit;
          elsif i = slv'length then
            errmes ("encounted a ""."" at the beginning of the line");
            xgood := false;
            exit;
          elsif founddot then
            errmes ("Two ""."" encounted in input string");
            xgood := false;
            exit;
          end if;
          founddot := true;
          lastu := false;
        else
          Char2TriBits(c, nybble, xgood, message);
          if not xgood then
            exit;
          end if;
          slv (i downto i-2) := nybble;
          i := i - 3;
          lastu := false;
        end if;
        if i > 0 then
          read (L, c, xgood);
        end if;
      end loop;
      idex := i;
      igood := xgood;
    else
      igood := true;                  -- read into a null array
      idex := -1;
    end if;
  end procedure OREAD_common;

  -- Note that for Octal and Hex read, you can not start with a ".",
  -- the read is for numbers formatted "A.BC".  These routines go to
  -- the nearest bounds, so "F.E" will fit into an sfixed (2 downto -3).
  procedure OREAD (L     : inout LINE;
                   VALUE : out   UNRESOLVED_ufixed) is
    constant hbv    : INTEGER := (((maximum(3, (VALUE'high+1))+2)/3)*3)-1;
    constant lbv    : INTEGER := ((mine(0, VALUE'low)-2)/3)*3;
    variable slv    : STD_ULOGIC_VECTOR (hbv-lbv downto 0);  -- high bits
    variable valuex : UNRESOLVED_ufixed (hbv downto lbv);
    variable igood  : BOOLEAN;
    variable i      : INTEGER;
  begin
    VALUE := (VALUE'range => 'U');
    OREAD_common ( L => L,
                   slv => slv,
                   igood => igood,
                   idex => i,
                   bpoint => -lbv,
                   message => true,
                   smath => false);
    if igood then                       -- We did not get another error
      if not ((i = -1) and               -- We read everything, and high bits 0
              (or (slv(hbv-lbv downto VALUE'high+1-lbv)) = '0')) then
        report fixed_generic_pkg'instance_name
          & "OREAD(ufixed): Vector truncated."
          severity error;
      else
        if (or (slv(VALUE'low-lbv-1 downto 0)) = '1') then
          assert no_warning
            report fixed_generic_pkg'instance_name
            & "OREAD(ufixed): Vector truncated"
            severity warning;
        end if;
        valuex := to_ufixed (slv, hbv, lbv);
        VALUE  := valuex (VALUE'range);
      end if;
    end if;
  end procedure OREAD;

  procedure OREAD(L     : inout LINE;
                  VALUE : out   UNRESOLVED_ufixed;
                  GOOD  : out   BOOLEAN) is
    constant hbv    : INTEGER := (((maximum(3, (VALUE'high+1))+2)/3)*3)-1;
    constant lbv    : INTEGER := ((mine(0, VALUE'low)-2)/3)*3;
    variable slv    : STD_ULOGIC_VECTOR (hbv-lbv downto 0);  -- high bits
    variable valuex : UNRESOLVED_ufixed (hbv downto lbv);
    variable igood  : BOOLEAN;
    variable i      : INTEGER;
  begin
    VALUE := (VALUE'range => 'U');
    OREAD_common ( L => L,
                   slv => slv,
                   igood => igood,
                   idex => i,
                   bpoint => -lbv,
                   message => false,
                   smath => false);
    if (igood and                   -- We did not get another error
        (i = -1) and                -- We read everything, and high bits 0
        (or (slv(hbv-lbv downto VALUE'high+1-lbv)) = '0')) then
      valuex := to_ufixed (slv, hbv, lbv);
      VALUE  := valuex (VALUE'range);
      GOOD := true;
    else
      GOOD := false;
    end if;
  end procedure OREAD;

  procedure OREAD(L     : inout LINE;
                  VALUE : out   UNRESOLVED_sfixed) is
    constant hbv    : INTEGER := (((maximum(3, (VALUE'high+1))+2)/3)*3)-1;
    constant lbv    : INTEGER := ((mine(0, VALUE'low)-2)/3)*3;
    variable slv    : STD_ULOGIC_VECTOR (hbv-lbv downto 0);  -- high bits
    variable valuex : UNRESOLVED_sfixed (hbv downto lbv);
    variable igood  : BOOLEAN;
    variable i      : INTEGER;
  begin
    VALUE := (VALUE'range => 'U');
    OREAD_common ( L => L,
                   slv => slv,
                   igood => igood,
                   idex => i,
                   bpoint => -lbv,
                   message => true,
                   smath => true);
    if igood then                       -- We did not get another error
      if not ((i = -1) and               -- We read everything
              ((slv(VALUE'high-lbv) = '0' and      -- sign bits = extra bits
                or (slv(hbv-lbv downto VALUE'high+1-lbv)) = '0') or
               (slv(VALUE'high-lbv) = '1' and
                and (slv(hbv-lbv downto VALUE'high+1-lbv)) = '1'))) then
        report fixed_generic_pkg'instance_name
          & "OREAD(sfixed): Vector truncated."
          severity error;
      else
        if (or (slv(VALUE'low-lbv-1 downto 0)) = '1') then
          assert no_warning
            report fixed_generic_pkg'instance_name
            & "OREAD(sfixed): Vector truncated"
            severity warning;
        end if;
        valuex := to_sfixed (slv, hbv, lbv);
        VALUE  := valuex (VALUE'range);
      end if;
    end if;
  end procedure OREAD;

  procedure OREAD(L     : inout LINE;
                  VALUE : out   UNRESOLVED_sfixed;
                  GOOD  : out   BOOLEAN) is
    constant hbv    : INTEGER := (((maximum(3, (VALUE'high+1))+2)/3)*3)-1;
    constant lbv    : INTEGER := ((mine(0, VALUE'low)-2)/3)*3;
    variable slv    : STD_ULOGIC_VECTOR (hbv-lbv downto 0);  -- high bits
    variable valuex : UNRESOLVED_sfixed (hbv downto lbv);
    variable igood  : BOOLEAN;
    variable i      : INTEGER;
  begin
    VALUE := (VALUE'range => 'U');
    OREAD_common ( L => L,
                   slv => slv,
                   igood => igood,
                   idex => i,
                   bpoint => -lbv,
                   message => false,
                   smath => true);
    if (igood                       -- We did not get another error
        and (i = -1)                -- We read everything
        and ((slv(VALUE'high-lbv) = '0' and  -- sign bits = extra bits
              or (slv(hbv-lbv downto VALUE'high+1-lbv)) = '0') or
             (slv(VALUE'high-lbv) = '1' and
              and (slv(hbv-lbv downto VALUE'high+1-lbv)) = '1'))) then
      valuex := to_sfixed (slv, hbv, lbv);
      VALUE  := valuex (VALUE'range);
      GOOD := true;
    else
      GOOD := false;
    end if;
  end procedure OREAD;

  -- hex read and write
  procedure hwrite (
    L         : inout LINE;               -- input line
    VALUE     : in    UNRESOLVED_ufixed;  -- fixed point input
    JUSTIFIED : in    SIDE  := right;
    FIELD     : in    WIDTH := 0) is
  begin  -- Example 03.30
    write (L         => L,
           VALUE     => TO_HSTRING (VALUE),
           JUSTIFIED => JUSTIFIED,
           FIELD     => FIELD);
  end procedure hwrite;

  -- purpose: writes fixed point into a line
  procedure hwrite (
    L         : inout LINE;               -- input line
    VALUE     : in    UNRESOLVED_sfixed;  -- fixed point input
    JUSTIFIED : in    SIDE  := right;
    FIELD     : in    WIDTH := 0) is
  begin  -- Example 03.30
    write (L         => L,
           VALUE     => TO_HSTRING (VALUE),
           JUSTIFIED => JUSTIFIED,
           FIELD     => FIELD);
  end procedure hwrite;

  -- Hex Read and Write procedures for STD_ULOGIC_VECTOR.
  -- Modified from the original to be more forgiving.

  procedure Char2QuadBits (C           :     CHARACTER;
                           RESULT      : out STD_ULOGIC_VECTOR(3 downto 0);
                           GOOD        : out BOOLEAN;
                           ISSUE_ERROR : in  BOOLEAN) is
  begin
    case C is
      when '0'       => RESULT := x"0"; GOOD := true;
      when '1'       => RESULT := x"1"; GOOD := true;
      when '2'       => RESULT := x"2"; GOOD := true;
      when '3'       => RESULT := x"3"; GOOD := true;
      when '4'       => RESULT := x"4"; GOOD := true;
      when '5'       => RESULT := x"5"; GOOD := true;
      when '6'       => RESULT := x"6"; GOOD := true;
      when '7'       => RESULT := x"7"; GOOD := true;
      when '8'       => RESULT := x"8"; GOOD := true;
      when '9'       => RESULT := x"9"; GOOD := true;
      when 'A' | 'a' => RESULT := x"A"; GOOD := true;
      when 'B' | 'b' => RESULT := x"B"; GOOD := true;
      when 'C' | 'c' => RESULT := x"C"; GOOD := true;
      when 'D' | 'd' => RESULT := x"D"; GOOD := true;
      when 'E' | 'e' => RESULT := x"E"; GOOD := true;
      when 'F' | 'f' => RESULT := x"F"; GOOD := true;
      when 'Z'       => RESULT := "ZZZZ"; GOOD := true;
      when 'X'       => RESULT := "XXXX"; GOOD := true;
      when others =>
        assert not ISSUE_ERROR
          report fixed_generic_pkg'instance_name
          & "HREAD Error: Read a '" & C &
          "', expected a Hex character (0-F)."
          severity error;
        RESULT := "UUUU";
        GOOD   := false;
    end case;
  end procedure Char2QuadBits;

  -- purpose: Routines common to the HREAD routines
  procedure HREAD_common (
    L                : inout LINE;
    slv              : out   STD_ULOGIC_VECTOR;
    igood            : out   BOOLEAN;
    idex             : out INTEGER;
    constant bpoint : in INTEGER;       -- binary point
    constant message : in    BOOLEAN;
    constant smath   : in    BOOLEAN) is

    -- purpose: error message routine
    procedure errmes (
      constant mess : in STRING) is     -- error message
    begin
      if message then
        if smath then
          report fixed_generic_pkg'instance_name
            & "HREAD(sfixed) "
            & mess
            severity error;
        else
          report fixed_generic_pkg'instance_name
            & "HREAD(ufixed) "
            & mess
            severity error;
        end if;
      end if;
    end procedure errmes;
    variable xgood  : BOOLEAN;
    variable nybble : STD_ULOGIC_VECTOR (3 downto 0);        -- 4 bits
    variable c      : CHARACTER;
    variable i      : INTEGER;
    variable lastu  : BOOLEAN := false;       -- last character was an "_"
    variable founddot : BOOLEAN := false;  -- found a dot.
  begin
    skip_whitespace (L);
    if slv'length > 0 then
      i := slv'high;
      read (L, c, xgood);
      while i > 0 loop
        if xgood = false then
          errmes ("Error: end of string encountered");
          exit;
        elsif c = '_' then
          if i = slv'length then
            errmes ("Error: String begins with an ""_""");
            xgood := false;
            exit;
          elsif lastu then
            errmes ("Error: Two underscores detected in input string ""__""");
            xgood := false;
            exit;
          else
            lastu := true;
          end if;
        elsif (c = '.') then
          if (i + 1 /= bpoint) then
            errmes ("encountered ""."" at wrong index");
            xgood := false;
            exit;
          elsif i = slv'length then
            errmes ("encounted a ""."" at the beginning of the line");
            xgood := false;
            exit;
          elsif founddot then
            errmes ("Two ""."" encounted in input string");
            xgood := false;
            exit;
          end if;
          founddot := true;
          lastu := false;
        else
          Char2QuadBits(c, nybble, xgood, message);
          if not xgood then
            exit;
          end if;
          slv (i downto i-3) := nybble;
          i := i - 4;
          lastu := false;
        end if;
        if i > 0 then
          read (L, c, xgood);
        end if;
      end loop;
      idex := i;
      igood := xgood;
    else
      idex := -1;
      igood := true;                    -- read null string
    end if;
  end procedure HREAD_common;

  procedure HREAD(L     : inout LINE;
                  VALUE : out   UNRESOLVED_ufixed) is
    constant hbv    : INTEGER := (((maximum(4, (VALUE'high+1))+3)/4)*4)-1;
    constant lbv    : INTEGER := ((mine(0, VALUE'low)-3)/4)*4;
    variable slv    : STD_ULOGIC_VECTOR (hbv-lbv downto 0);  -- high bits
    variable valuex : UNRESOLVED_ufixed (hbv downto lbv);
    variable igood  : BOOLEAN;
    variable i      : INTEGER;
  begin
    VALUE := (VALUE'range => 'U');
    HREAD_common ( L => L,
                   slv => slv,
                   igood => igood,
                   idex => i,
                   bpoint => -lbv,
                   message => true,
                   smath => false);
    if igood then
      if not ((i = -1) and               -- We read everything, and high bits 0
              (or (slv(hbv-lbv downto VALUE'high+1-lbv)) = '0')) then
        report fixed_generic_pkg'instance_name
          & "HREAD(ufixed): Vector truncated."
          severity error;
      else
        if (or (slv(VALUE'low-lbv-1 downto 0)) = '1') then
          assert no_warning
            report fixed_generic_pkg'instance_name
            & "HREAD(ufixed): Vector truncated"
            severity warning;
        end if;
        valuex := to_ufixed (slv, hbv, lbv);
        VALUE  := valuex (VALUE'range);
      end if;
    end if;
  end procedure HREAD;

  procedure HREAD(L     : inout LINE;
                  VALUE : out   UNRESOLVED_ufixed;
                  GOOD  : out   BOOLEAN) is
    constant hbv    : INTEGER := (((maximum(4, (VALUE'high+1))+3)/4)*4)-1;
    constant lbv    : INTEGER := ((mine(0, VALUE'low)-3)/4)*4;
    variable slv    : STD_ULOGIC_VECTOR (hbv-lbv downto 0);  -- high bits
    variable valuex : UNRESOLVED_ufixed (hbv downto lbv);
    variable igood  : BOOLEAN;
    variable i      : INTEGER;
  begin
    VALUE := (VALUE'range => 'U');
    HREAD_common ( L => L,
                   slv => slv,
                   igood => igood,
                   idex => i,
                   bpoint => -lbv,
                   message => false,
                   smath => false);
    if (igood and                   -- We did not get another error
        (i = -1) and                -- We read everything, and high bits 0
        (or (slv(hbv-lbv downto VALUE'high+1-lbv)) = '0')) then
      valuex := to_ufixed (slv, hbv, lbv);
      VALUE  := valuex (VALUE'range);
      GOOD := true;
    else
      GOOD := false;
    end if;
  end procedure HREAD;

  procedure HREAD(L     : inout LINE;
                  VALUE : out   UNRESOLVED_sfixed) is
    constant hbv    : INTEGER := (((maximum(4, (VALUE'high+1))+3)/4)*4)-1;
    constant lbv    : INTEGER := ((mine(0, VALUE'low)-3)/4)*4;
    variable slv    : STD_ULOGIC_VECTOR (hbv-lbv downto 0);  -- high bits
    variable valuex : UNRESOLVED_sfixed (hbv downto lbv);
    variable igood  : BOOLEAN;
    variable i      : INTEGER;
  begin
    VALUE := (VALUE'range => 'U');
    HREAD_common ( L => L,
                   slv => slv,
                   igood => igood,
                   idex => i,
                   bpoint => -lbv,
                   message => true,
                   smath => true);
    if igood then                       -- We did not get another error
      if not ((i = -1)                   -- We read everything
              and ((slv(VALUE'high-lbv) = '0' and  -- sign bits = extra bits
                    or (slv(hbv-lbv downto VALUE'high+1-lbv)) = '0') or
                   (slv(VALUE'high-lbv) = '1' and
                    and (slv(hbv-lbv downto VALUE'high+1-lbv)) = '1'))) then
        report fixed_generic_pkg'instance_name
          & "HREAD(sfixed): Vector truncated."
          severity error;
      else
        if (or (slv(VALUE'low-lbv-1 downto 0)) = '1') then
          assert no_warning
            report fixed_generic_pkg'instance_name
            & "HREAD(sfixed): Vector truncated"
            severity warning;
        end if;
        valuex := to_sfixed (slv, hbv, lbv);
        VALUE  := valuex (VALUE'range);
      end if;
    end if;
  end procedure HREAD;

  procedure HREAD(L     : inout LINE;
                  VALUE : out   UNRESOLVED_sfixed;
                  GOOD  : out   BOOLEAN) is
    constant hbv    : INTEGER := (((maximum(4, (VALUE'high+1))+3)/4)*4)-1;
    constant lbv    : INTEGER := ((mine(0, VALUE'low)-3)/4)*4;
    variable slv    : STD_ULOGIC_VECTOR (hbv-lbv downto 0);  -- high bits
    variable valuex : UNRESOLVED_sfixed (hbv downto lbv);
    variable igood  : BOOLEAN;
    variable i      : INTEGER;
  begin
    VALUE := (VALUE'range => 'U');
    HREAD_common ( L => L,
                   slv => slv,
                   igood => igood,
                   idex => i,
                   bpoint => -lbv,
                   message => false,
                   smath => true);
    if (igood and                   -- We did not get another error
        (i = -1) and                -- We read everything
        ((slv(VALUE'high-lbv) = '0' and  -- sign bits = extra bits
          or (slv(hbv-lbv downto VALUE'high+1-lbv)) = '0') or
         (slv(VALUE'high-lbv) = '1' and
          and (slv(hbv-lbv downto VALUE'high+1-lbv)) = '1'))) then
      valuex := to_sfixed (slv, hbv, lbv);
      VALUE  := valuex (VALUE'range);
      GOOD := true;
    else
      GOOD := false;
    end if;
  end procedure HREAD;

  -- TO_STRING functions.  Useful in "report" statements.
  -- Example:  report "result was " & TO_STRING(result);
  function TO_STRING (value : UNRESOLVED_ufixed) return STRING is
    variable s     : STRING(1 to value'length +1) := (others => ' ');
    variable subval : UNRESOLVED_ufixed (value'high downto -1);
    variable sindx : INTEGER;
  begin
    if value'length < 1 then
      return NUS;
    else
      if value'high < 0 then
        if value(value'high) = 'Z' then
          return TO_STRING (resize (sfixed(value), 0, value'low));
        else
          return TO_STRING (resize (value, 0, value'low));
        end if;
      elsif value'low >= 0 then
        if Is_X (value(value'low)) then
          subval := (others => value(value'low));
          subval (value'range) := value;
          return TO_STRING(subval);
        else
          return TO_STRING (resize (value, value'high, -1));
        end if;
      else
        sindx := 1;
        for i in value'high downto value'low loop
          if i = -1 then
            s(sindx) := '.';
            sindx    := sindx + 1;
          end if;
          s(sindx) := MVL9_to_char(STD_ULOGIC(value(i)));
          sindx    := sindx + 1;
        end loop;
        return s;
      end if;
    end if;
  end function TO_STRING;

  function TO_STRING (value : UNRESOLVED_sfixed) return STRING is
    variable s     : STRING(1 to value'length + 1) := (others => ' ');
    variable subval : UNRESOLVED_sfixed (value'high downto -1);
    variable sindx : INTEGER;
  begin
    if value'length < 1 then
      return NUS;
    else
      if value'high < 0 then
        return TO_STRING (resize (value, 0, value'low));
      elsif value'low >= 0 then
        if Is_X (value(value'low)) then
          subval := (others => value(value'low));
          subval (value'range) := value;
          return TO_STRING(subval);
        else
          return TO_STRING (resize (value, value'high, -1));
        end if;
      else
        sindx := 1;
        for i in value'high downto value'low loop
          if i = -1 then
            s(sindx) := '.';
            sindx    := sindx + 1;
          end if;
          s(sindx) := MVL9_to_char(STD_ULOGIC(value(i)));
          sindx    := sindx + 1;
        end loop;
        return s;
      end if;
    end if;
  end function TO_STRING;

  function TO_OSTRING (value : UNRESOLVED_ufixed) return STRING is
    constant lne  : INTEGER := (-value'low+2)/3;
    variable subval : UNRESOLVED_ufixed (value'high downto -3);
    variable lpad : STD_ULOGIC_VECTOR (0 to (lne*3 + value'low) -1);
    variable slv : STD_ULOGIC_VECTOR (value'length-1 downto 0);
  begin
    if value'length < 1 then
      return NUS;
    else
      if value'high < 0 then
        if value(value'high) = 'Z' then
          return TO_OSTRING (resize (sfixed(value), 2, value'low));
        else
          return TO_OSTRING (resize (value, 2, value'low));
        end if;
      elsif value'low >= 0 then
        if Is_X (value(value'low)) then
          subval := (others => value(value'low));
          subval (value'range) := value;
          return TO_OSTRING(subval);
        else
          return TO_OSTRING (resize (value, value'high, -3));
        end if;
      else
        slv := to_sulv (value);
        if Is_X (value (value'low)) then
          lpad := (others => value (value'low));
        else
          lpad := (others => '0');
        end if;
        return TO_OSTRING(slv(slv'high downto slv'high-value'high))
          & "."
          & TO_OSTRING(slv(slv'high-value'high-1 downto 0) & lpad);
      end if;
    end if;
  end function TO_OSTRING;

  function TO_HSTRING (value : UNRESOLVED_ufixed) return STRING is
    constant lne  : INTEGER := (-value'low+3)/4;
    variable subval : UNRESOLVED_ufixed (value'high downto -4);
    variable lpad : STD_ULOGIC_VECTOR (0 to (lne*4 + value'low) -1);
    variable slv : STD_ULOGIC_VECTOR (value'length-1 downto 0);
  begin
    if value'length < 1 then
      return NUS;
    else
      if value'high < 0 then
        if value(value'high) = 'Z' then
          return TO_HSTRING (resize (sfixed(value), 3, value'low));
        else
          return TO_HSTRING (resize (value, 3, value'low));
        end if;
      elsif value'low >= 0 then
        if Is_X (value(value'low)) then
          subval := (others => value(value'low));
          subval (value'range) := value;
          return TO_HSTRING(subval);
        else
          return TO_HSTRING (resize (value, value'high, -4));
        end if;
      else
        slv := to_sulv (value);
        if Is_X (value (value'low)) then
          lpad := (others => value(value'low));
        else
          lpad := (others => '0');
        end if;
        return TO_HSTRING(slv(slv'high downto slv'high-value'high))
          & "."
          & TO_HSTRING(slv(slv'high-value'high-1 downto 0)&lpad);
      end if;
    end if;
  end function TO_HSTRING;

  function TO_OSTRING (value : UNRESOLVED_sfixed) return STRING is
    constant ne   : INTEGER := ((value'high+1)+2)/3;
    variable pad  : STD_ULOGIC_VECTOR(0 to (ne*3 - (value'high+1)) - 1);
    constant lne  : INTEGER := (-value'low+2)/3;
    variable subval : UNRESOLVED_sfixed (value'high downto -3);
    variable lpad : STD_ULOGIC_VECTOR (0 to (lne*3 + value'low) -1);
    variable slv  : STD_ULOGIC_VECTOR (value'high - value'low downto 0);
  begin
    if value'length < 1 then
      return NUS;
    else
      if value'high < 0 then
        return TO_OSTRING (resize (value, 2, value'low));
      elsif value'low >= 0 then
        if Is_X (value(value'low)) then
          subval := (others => value(value'low));
          subval (value'range) := value;
          return TO_OSTRING(subval);
        else
          return TO_OSTRING (resize (value, value'high, -3));
        end if;
      else
        pad := (others => value(value'high));
        slv := to_sulv (value);
        if Is_X (value (value'low)) then
          lpad := (others => value(value'low));
        else
          lpad := (others => '0');
        end if;
        return TO_OSTRING(pad & slv(slv'high downto slv'high-value'high))
          & "."
          & TO_OSTRING(slv(slv'high-value'high-1 downto 0) & lpad);
      end if;
    end if;
  end function TO_OSTRING;

  function TO_HSTRING (value : UNRESOLVED_sfixed) return STRING is
    constant ne   : INTEGER := ((value'high+1)+3)/4;
    variable pad  : STD_ULOGIC_VECTOR(0 to (ne*4 - (value'high+1)) - 1);
    constant lne  : INTEGER := (-value'low+3)/4;
    variable subval : UNRESOLVED_sfixed (value'high downto -4);
    variable lpad : STD_ULOGIC_VECTOR (0 to (lne*4 + value'low) -1);
    variable slv  : STD_ULOGIC_VECTOR (value'length-1 downto 0);
  begin
    if value'length < 1 then
      return NUS;
    else
      if value'high < 0 then
        return TO_HSTRING (resize (value, 3, value'low));
      elsif value'low >= 0 then
        if Is_X (value(value'low)) then
          subval := (others => value(value'low));
          subval (value'range) := value;
          return TO_HSTRING(subval);
        else
          return TO_HSTRING (resize (value, value'high, -4));
        end if;
      else
        slv := to_sulv (value);
        pad := (others => value(value'high));
        if Is_X (value (value'low)) then
          lpad := (others => value(value'low));
        else
          lpad := (others => '0');
        end if;
        return TO_HSTRING(pad & slv(slv'high downto slv'high-value'high))
          & "."
          & TO_HSTRING(slv(slv'high-value'high-1 downto 0) & lpad);
      end if;
    end if;
  end function TO_HSTRING;

  -- From string functions allow you to convert a string into a fixed
  -- point number.  Example:
  --  signal uf1 : ufixed (3 downto -3);
  --  uf1 <= from_string ("0110.100", uf1'high, uf1'low); -- 6.5
  -- The "." is optional in this syntax, however it exist and is
  -- in the wrong location an error is produced.  Overflow will
  -- result in saturation.
  function from_string (
    bstring              : STRING;      -- binary string
    constant left_index  : INTEGER;
    constant right_index : INTEGER)
    return UNRESOLVED_ufixed
  is
    variable result : UNRESOLVED_ufixed (left_index downto right_index);
    variable L      : LINE;
    variable good   : BOOLEAN;
  begin
    L := new STRING'(bstring);
    READ (L, result, good);
    deallocate (L);
    assert (good)
      report fixed_generic_pkg'instance_name
      & "from_string: Bad string "& bstring severity error;
    return result;
  end function from_string;

  -- Octal and hex conversions work as follows:
  -- uf1 <= from_hstring ("6.8", 3, -3); -- 6.5 (bottom zeros dropped)
  -- uf1 <= from_ostring ("06.4", 3, -3); -- 6.5 (top zeros dropped)
  function from_ostring (
    ostring              : STRING;      -- Octal string
    constant left_index  : INTEGER;
    constant right_index : INTEGER)
    return UNRESOLVED_ufixed
  is
    variable result : UNRESOLVED_ufixed (left_index downto right_index);
    variable L      : LINE;
    variable good   : BOOLEAN;
  begin
    L := new STRING'(ostring);
    OREAD (L, result, good);
    deallocate (L);
    assert (good)
      report fixed_generic_pkg'instance_name
      & "from_ostring: Bad string "& ostring severity error;
    return result;
  end function from_ostring;

  function from_hstring (
    hstring              : STRING;      -- hex string
    constant left_index  : INTEGER;
    constant right_index : INTEGER)
    return UNRESOLVED_ufixed
  is
    variable result : UNRESOLVED_ufixed (left_index downto right_index);
    variable L      : LINE;
    variable good   : BOOLEAN;
  begin
    L := new STRING'(hstring);
    HREAD (L, result, good);
    deallocate (L);
    assert (good)
      report fixed_generic_pkg'instance_name
      & "from_hstring: Bad string "& hstring severity error;
    return result;
  end function from_hstring;

  function from_string (
    bstring              : STRING;      -- binary string
    constant left_index  : INTEGER;
    constant right_index : INTEGER)
    return UNRESOLVED_sfixed
  is
    variable result : UNRESOLVED_sfixed (left_index downto right_index);
    variable L      : LINE;
    variable good   : BOOLEAN;
  begin
    L := new STRING'(bstring);
    READ (L, result, good);
    deallocate (L);
    assert (good)
      report fixed_generic_pkg'instance_name
      & "from_string: Bad string "& bstring severity error;
    return result;
  end function from_string;

  function from_ostring (
    ostring              : STRING;      -- Octal string
    constant left_index  : INTEGER;
    constant right_index : INTEGER)
    return UNRESOLVED_sfixed
  is
    variable result : UNRESOLVED_sfixed (left_index downto right_index);
    variable L      : LINE;
    variable good   : BOOLEAN;
  begin
    L := new STRING'(ostring);
    OREAD (L, result, good);
    deallocate (L);
    assert (good)
      report fixed_generic_pkg'instance_name
      & "from_ostring: Bad string "& ostring severity error;
    return result;
  end function from_ostring;

  function from_hstring (
    hstring              : STRING;      -- hex string
    constant left_index  : INTEGER;
    constant right_index : INTEGER)
    return UNRESOLVED_sfixed
  is
    variable result : UNRESOLVED_sfixed (left_index downto right_index);
    variable L      : LINE;
    variable good   : BOOLEAN;
  begin
    L := new STRING'(hstring);
    HREAD (L, result, good);
    deallocate (L);
    assert (good)
      report fixed_generic_pkg'instance_name
      & "from_hstring: Bad string "& hstring severity error;
    return result;
  end function from_hstring;

  -- Same as above, "size_res" is used for it's range only.
 function from_string (
    bstring  : STRING;                  -- binary string
    size_res : UNRESOLVED_ufixed)
    return UNRESOLVED_ufixed is
  begin
    return from_string (bstring, size_res'high, size_res'low);
  end function from_string;

  function from_ostring (
    ostring  : STRING;                  -- Octal string
    size_res : UNRESOLVED_ufixed)
    return UNRESOLVED_ufixed is
  begin
    return from_ostring (ostring, size_res'high, size_res'low);
  end function from_ostring;

  function from_hstring (
    hstring  : STRING;                  -- hex string
    size_res : UNRESOLVED_ufixed)
    return UNRESOLVED_ufixed is
  begin
    return from_hstring(hstring, size_res'high, size_res'low);
  end function from_hstring;

  function from_string (
    bstring  : STRING;                  -- binary string
    size_res : UNRESOLVED_sfixed)
    return UNRESOLVED_sfixed is
  begin
    return from_string (bstring, size_res'high, size_res'low);
  end function from_string;

  function from_ostring (
    ostring  : STRING;                  -- Octal string
    size_res : UNRESOLVED_sfixed)
    return UNRESOLVED_sfixed is
  begin
    return from_ostring (ostring, size_res'high, size_res'low);
  end function from_ostring;

  function from_hstring (
    hstring  : STRING;                  -- hex string
    size_res : UNRESOLVED_sfixed)
    return UNRESOLVED_sfixed is
  begin
    return from_hstring (hstring, size_res'high, size_res'low);
  end function from_hstring;

  -- Direct conversion functions.  Example:
  --  signal uf1 : ufixed (3 downto -3);
  --  uf1 <= from_string ("0110.100"); -- 6.5
  -- In this case the "." is not optional, and the size of
  -- the output must match exactly.
  -- purpose: Calculate the string boundaries
  procedure calculate_string_boundry (
    arg         : in  STRING;           -- input string
    left_index  : out INTEGER;          -- left
    right_index : out INTEGER) is       -- right
    -- examples "10001.111" would return +4, -3
    -- "07X.44" would return +2, -2 (then the octal routine would multiply)
    -- "A_B_._C" would return +1, -1 (then the hex routine would multiply)
    alias xarg : STRING (arg'length downto 1) is arg;  -- make it downto range
    variable l, r : INTEGER;            -- internal indexes
    variable founddot : BOOLEAN := false;
  begin
    if arg'length > 0 then
      l := xarg'high - 1;
      r := 0;
      for i in xarg'range loop
        if xarg(i) = '_' then
          if r = 0 then
            l := l - 1;
          else
            r := r + 1;
          end if;
        elsif xarg(i) = ' ' or xarg(i) = NBSP or xarg(i) = HT then
          report fixed_generic_pkg'instance_name
            & "Found a space in the input STRING " & xarg
            severity error;
        elsif xarg(i) = '.' then
          if founddot then
            report fixed_generic_pkg'instance_name
              & "Found two binary points in input string " & xarg
              severity error;
          else
            l := l - i;
            r := -i + 1;
            founddot := true;
          end if;
        end if;
      end loop;
      left_index := l;
      right_index := r;
    else
      left_index := 0;
      right_index := 0;
    end if;
  end procedure calculate_string_boundry;

  -- Direct conversion functions.  Example:
  --  signal uf1 : ufixed (3 downto -3);
  --  uf1 <= from_string ("0110.100"); -- 6.5
  -- In this case the "." is not optional, and the size of
  -- the output must match exactly.
  function from_string (
    bstring : STRING)                      -- binary string
    return UNRESOLVED_ufixed
  is
    variable left_index, right_index : INTEGER;
  begin
    calculate_string_boundry (bstring, left_index, right_index);
    return from_string (bstring, left_index, right_index);
  end function from_string;

  -- Direct octal and hex conversion functions.  In this case
  -- the string lengths must match.  Example:
  -- signal sf1 := sfixed (5 downto -3);
  -- sf1 <= from_ostring ("71.4") -- -6.5
  function from_ostring (
    ostring : STRING)                      -- Octal string
    return UNRESOLVED_ufixed
  is
    variable left_index, right_index : INTEGER;
  begin
    calculate_string_boundry (ostring, left_index, right_index);
    return from_ostring (ostring, ((left_index+1)*3)-1, right_index*3);
  end function from_ostring;

  function from_hstring (
    hstring : STRING)                      -- hex string
    return UNRESOLVED_ufixed
  is
    variable left_index, right_index : INTEGER;
  begin
    calculate_string_boundry (hstring, left_index, right_index);
    return from_hstring (hstring, ((left_index+1)*4)-1, right_index*4);
  end function from_hstring;

  function from_string (
    bstring : STRING)                      -- binary string
    return UNRESOLVED_sfixed
  is
    variable left_index, right_index : INTEGER;
  begin
    calculate_string_boundry (bstring, left_index, right_index);
    return from_string (bstring, left_index, right_index);
  end function from_string;

  function from_ostring (
    ostring : STRING)                      -- Octal string
    return UNRESOLVED_sfixed
  is
    variable left_index, right_index : INTEGER;
  begin
    calculate_string_boundry (ostring, left_index, right_index);
    return from_ostring (ostring, ((left_index+1)*3)-1, right_index*3);
  end function from_ostring;

  function from_hstring (
    hstring : STRING)                      -- hex string
    return UNRESOLVED_sfixed
  is
    variable left_index, right_index : INTEGER;
  begin
    calculate_string_boundry (hstring, left_index, right_index);
    return from_hstring (hstring, ((left_index+1)*4)-1, right_index*4);
  end function from_hstring;

end package body fixed_generic_pkg;
