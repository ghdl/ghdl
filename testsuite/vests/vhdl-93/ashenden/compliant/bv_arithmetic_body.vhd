
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
-- $Id: bv_arithmetic_body.vhd,v 1.3 2001-10-26 16:29:33 paw Exp $
-- $Revision: 1.3 $
--
-- ---------------------------------------------------------------------

package body bv_arithmetic is

  ----------------------------------------------------------------
  --  Type conversions
  ----------------------------------------------------------------

  function bv_to_natural ( bv : in bit_vector ) return natural is

    variable result : natural := 0;

  begin
    for index in bv'range loop
      result := result * 2 + bit'pos( bv(index) );
    end loop;
    return result;
  end function bv_to_natural;

  function natural_to_bv ( nat : in natural;
      	      	      	   length : in natural ) return bit_vector is

    variable temp : natural := nat;
    variable result : bit_vector(length - 1 downto 0) := (others => '0');

  begin
    for index in result'reverse_range loop
      result(index) := bit'val( temp rem 2 );
      temp := temp / 2;
      exit when temp = 0;
    end loop;
    return result;
  end function natural_to_bv;

  function bv_to_integer ( bv : in bit_vector ) return integer is

    variable temp : bit_vector(bv'range);
    variable result : integer := 0;

  begin
    if bv(bv'left) = '1' then	  -- negative number
      temp := not bv;
    else
      temp := bv;
    end if;
    for index in bv'range loop	  -- sign bit of temp = '0'
      result := result * 2 + bit'pos( temp(index) );
    end loop;
    if bv(bv'left) = '1' then
      result := (-result) - 1;
    end if;
    return result;
  end function bv_to_integer;

  function integer_to_bv ( int : in integer;
      	      	      	   length : in natural ) return bit_vector is

    variable temp : integer;
    variable result : bit_vector(length - 1 downto 0) := (others => '0');

  begin
    if int < 0 then
      temp := - (int + 1);
    else
      temp := int;
    end if;
    for index in result'reverse_range loop
      result(index) := bit'val( temp rem 2 );
      temp := temp / 2;
      exit when temp = 0;
    end loop;
    if int < 0 then
      result := not result;
      result(result'left) := '1';
    end if;
    return result;
  end function integer_to_bv;

  ----------------------------------------------------------------
  --  Arithmetic operations
  ----------------------------------------------------------------

  procedure bv_add ( bv1, bv2 : in bit_vector;
      	       	     bv_result : out bit_vector;
		     overflow : out boolean ) is

    alias op1 : bit_vector(bv1'length - 1 downto 0) is bv1;
    alias op2 : bit_vector(bv2'length - 1 downto 0) is bv2;  
    variable result : bit_vector(bv_result'length - 1 downto 0);
    variable carry_in : bit;
    variable carry_out : bit := '0';

  begin
    if bv1'length /= bv2'length or bv1'length /= bv_result'length then
      report "bv_add: operands of different lengths"
        severity failure;
    else
      for index in result'reverse_range loop
        carry_in := carry_out;  -- of previous bit
        result(index) := op1(index) xor op2(index) xor carry_in;
        carry_out := (op1(index) and op2(index))
                     or (carry_in and (op1(index) xor op2(index)));
      end loop;
      bv_result := result;
      overflow := carry_out /= carry_in;
    end if;
  end procedure bv_add;

  function "+" ( bv1, bv2 : in bit_vector ) return bit_vector is

    alias op1 : bit_vector(bv1'length - 1 downto 0) is bv1;
    alias op2 : bit_vector(bv2'length - 1 downto 0) is bv2;  
    variable result : bit_vector(bv1'length - 1 downto 0);
    variable carry_in : bit;
    variable carry_out : bit := '0';

  begin
    if bv1'length /= bv2'length then
      report """+"": operands of different lengths"
        severity failure;
    else
      for index in result'reverse_range loop
        carry_in := carry_out;  -- of previous bit
        result(index) := op1(index) xor op2(index) xor carry_in;
        carry_out := (op1(index) and op2(index))
                     or (carry_in and (op1(index) xor op2(index)));
      end loop;
    end if;
    return result;
  end function "+";

  procedure bv_sub ( bv1, bv2 : in bit_vector;
      	       	     bv_result : out bit_vector;
		     overflow : out boolean ) is

    -- subtraction implemented by adding ((not bv2) + 1), ie -bv2

    alias op1 : bit_vector(bv1'length - 1 downto 0) is bv1;
    alias op2 : bit_vector(bv2'length - 1 downto 0) is bv2;  
    variable result : bit_vector(bv_result'length - 1 downto 0);
    variable carry_in : bit;
    variable carry_out : bit := '1';

  begin
    if bv1'length /= bv2'length or bv1'length /= bv_result'length then
      report "bv_sub: operands of different lengths"
        severity failure;
    else
      for index in result'reverse_range loop
        carry_in := carry_out;  -- of previous bit
        result(index) := op1(index) xor (not op2(index)) xor carry_in;
        carry_out := (op1(index) and (not op2(index)))
                     or (carry_in and (op1(index) xor (not op2(index))));
      end loop;
      bv_result := result;
      overflow := carry_out /= carry_in;
    end if;
  end procedure bv_sub;

  function "-" ( bv1, bv2 : in bit_vector ) return bit_vector is

    -- subtraction implemented by adding ((not bv2) + 1), ie -bv2

    alias op1 : bit_vector(bv1'length - 1 downto 0) is bv1;
    alias op2 : bit_vector(bv2'length - 1 downto 0) is bv2;  
    variable result : bit_vector(bv1'length - 1 downto 0);
    variable carry_in : bit;
    variable carry_out : bit := '1';

  begin
    if bv1'length /= bv2'length then
      report """-"": operands of different lengths"
        severity failure;
    else
      for index in result'reverse_range loop
        carry_in := carry_out;  -- of previous bit
        result(index) := op1(index) xor (not op2(index)) xor carry_in;
        carry_out := (op1(index) and (not op2(index)))
                     or (carry_in and (op1(index) xor (not op2(index))));
      end loop;
    end if;
    return result;
  end function "-";

  procedure bv_addu ( bv1, bv2 : in bit_vector;
      	       	      bv_result : out bit_vector;
		      overflow : out boolean ) is

    alias op1 : bit_vector(bv1'length - 1 downto 0) is bv1;
    alias op2 : bit_vector(bv2'length - 1 downto 0) is bv2;  
    variable result : bit_vector(bv_result'length - 1 downto 0);
    variable carry : bit := '0';

  begin
    if bv1'length /= bv2'length or bv1'length /= bv_result'length then
      report "bv_addu: operands of different lengths"
        severity failure;
    else
      for index in result'reverse_range loop
        result(index) := op1(index) xor op2(index) xor carry;
        carry := (op1(index) and op2(index))
                 or (carry and (op1(index) xor op2(index)));
      end loop;
      bv_result := result;
      overflow := carry = '1';
    end if;
  end procedure bv_addu;

  function bv_addu ( bv1, bv2 : in bit_vector ) return bit_vector is

    alias op1 : bit_vector(bv1'length - 1 downto 0) is bv1;
    alias op2 : bit_vector(bv2'length - 1 downto 0) is bv2;  
    variable result : bit_vector(bv1'length - 1 downto 0);
    variable carry : bit := '0';

  begin
    if bv1'length /= bv2'length then
      report "bv_addu: operands of different lengths"
        severity failure;
    else
      for index in result'reverse_range loop
        result(index) := op1(index) xor op2(index) xor carry;
        carry := (op1(index) and op2(index))
                 or (carry and (op1(index) xor op2(index)));
      end loop;
    end if;
    return result;
  end function bv_addu;

  procedure bv_subu ( bv1, bv2 : in bit_vector;
      	       	      bv_result : out bit_vector;
		      overflow : out boolean ) is

    alias op1 : bit_vector(bv1'length - 1 downto 0) is bv1;
    alias op2 : bit_vector(bv2'length - 1 downto 0) is bv2;  
    variable result : bit_vector(bv_result'length - 1 downto 0);
    variable borrow : bit := '0';

  begin
    if bv1'length /= bv2'length or bv1'length /= bv_result'length then
      report "bv_subu: operands of different lengths"
        severity failure;
    else
      for index in result'reverse_range loop
        result(index) := op1(index) xor op2(index) xor borrow;
        borrow := (not op1(index) and op2(index))
                  or (borrow and not (op1(index) xor op2(index)));
      end loop;
      bv_result := result;
      overflow := borrow = '1';
    end if;
  end procedure bv_subu;

  function bv_subu ( bv1, bv2 : in bit_vector ) return bit_vector is

    alias op1 : bit_vector(bv1'length - 1 downto 0) is bv1;
    alias op2 : bit_vector(bv2'length - 1 downto 0) is bv2;  
    variable result : bit_vector(bv1'length - 1 downto 0);
    variable borrow : bit := '0';

  begin
    if bv1'length /= bv2'length then
      report "bv_subu: operands of different lengths"
        severity failure;
    else
      for index in result'reverse_range loop
        result(index) := op1(index) xor op2(index) xor borrow;
        borrow := (not op1(index) and op2(index))
                  or (borrow and not (op1(index) xor op2(index)));
      end loop;
    end if;
    return result;
  end function bv_subu;

  procedure bv_neg ( bv : in bit_vector;
                     bv_result : out bit_vector;
                     overflow : out boolean ) is

    constant zero : bit_vector(bv'range) := (others => '0');

  begin
    bv_sub( zero, bv, bv_result, overflow );
  end procedure bv_neg;


  function "-" ( bv : in bit_vector ) return bit_vector is

    constant zero : bit_vector(bv'range) := (others => '0');

  begin
    return zero - bv;
  end function "-";

  procedure bv_mult ( bv1, bv2 : in bit_vector;
      	       	      bv_result : out bit_vector;
		      overflow : out boolean ) is

    variable negative_result : boolean;
    variable op1 : bit_vector(bv1'range) := bv1;
    variable op2 : bit_vector(bv2'range) := bv2;
    variable multu_result : bit_vector(bv1'range);
    variable multu_overflow : boolean;
    variable abs_min_int : bit_vector(bv1'range) := (others => '0');

  begin
    if bv1'length /= bv2'length or bv1'length /= bv_result'length then
      report "bv_mult: operands of different lengths"
        severity failure;
    else
      abs_min_int(bv1'left) := '1';
      negative_result := (op1(op1'left) = '1') xor (op2(op2'left) = '1');
      if op1(op1'left) = '1' then
        op1 := - bv1;
      end if;
      if op2(op2'left) = '1' then
        op2 := - bv2;
      end if;
      bv_multu(op1, op2, multu_result, multu_overflow);
      if negative_result then
        overflow := multu_overflow or (multu_result > abs_min_int);
        bv_result := - multu_result;
      else
        overflow := multu_overflow or (multu_result(multu_result'left) = '1');
        bv_result := multu_result;
      end if;
    end if;
  end procedure bv_mult;

  function "*" ( bv1, bv2 : in bit_vector ) return bit_vector is

    variable negative_result : boolean;
    variable op1 : bit_vector(bv1'range) := bv1;
    variable op2 : bit_vector(bv2'range) := bv2;
    variable result : bit_vector(bv1'range);

  begin
    if bv1'length /= bv2'length then
      report """*"": operands of different lengths"
        severity failure;
    else
      negative_result := (op1(op1'left) = '1') xor (op2(op2'left) = '1');
      if op1(op1'left) = '1' then
        op1 := - bv1;
      end if;
      if op2(op2'left) = '1' then
        op2 := - bv2;
      end if;
      result := bv_multu(op1, op2);
      if negative_result then
        result := - result;
      end if;
    end if;
    return result;
  end function "*";

  procedure bv_multu ( bv1, bv2 : in bit_vector;
      	       	       bv_result : out bit_vector;
		       overflow : out boolean ) is

    alias op1 : bit_vector(bv1'length - 1 downto 0) is bv1;
    alias op2 : bit_vector(bv2'length - 1 downto 0) is bv2;  
    constant len : natural := bv1'length;
    constant accum_len : natural := len * 2;
    variable accum : bit_vector(accum_len - 1 downto 0) := (others => '0');
    constant zero : bit_vector(accum_len - 1 downto len):= (others => '0');
    variable addu_overflow : boolean;

  begin
    if bv1'length /= bv2'length or bv1'length /= bv_result'length then
      report "bv_multu: operands of different lengths"
        severity failure;
    else
      for count in 0 to len - 1 loop
        if op2(count) = '1' then
          bv_addu( accum(count + len - 1 downto count), op1,
                   accum(count + len - 1 downto count), addu_overflow);
          accum(count + len) := bit'val(boolean'pos(addu_overflow));
        end if;
      end loop;
      bv_result := accum(len - 1 downto 0);
      overflow := accum(accum_len-1 downto len) /= zero;
    end if;
  end procedure bv_multu;

  function bv_multu ( bv1, bv2 : in bit_vector ) return bit_vector is

    -- Use bv_multu with overflow detection, but ignore overflow flag

    variable result : bit_vector(bv1'range);
    variable tmp_overflow : boolean;

  begin
    bv_multu(bv1, bv2, result, tmp_overflow);
    return result;
  end function bv_multu;

  procedure bv_div ( bv1, bv2 : in bit_vector;
      	       	     bv_result : out bit_vector;
		     div_by_zero : out boolean;
                     overflow : out boolean ) is

    --  Need overflow, in case divide b"10...0" (min_int) by -1
    --  Don't use bv_to_int, in case size bigger than host machine!

    variable negative_result : boolean;
    variable op1 : bit_vector(bv1'range) := bv1;
    variable op2 : bit_vector(bv2'range) := bv2;
    variable divu_result : bit_vector(bv1'range);

  begin
    if bv1'length /= bv2'length or bv1'length /= bv_result'length then
      report "bv_div: operands of different lengths"
        severity failure;
    else
      negative_result := (op1(op1'left) = '1') xor (op2(op2'left) = '1');
      if op1(op1'left) = '1' then
        op1 := - bv1;
      end if;
      if op2(op2'left) = '1' then
        op2 := - bv2;
      end if;
      bv_divu(op1, op2, divu_result, div_by_zero);
      if negative_result then
        overflow := false;
        bv_result := - divu_result;
      else
        overflow := divu_result(divu_result'left) = '1';
        bv_result := divu_result;
      end if;
    end if;
  end procedure bv_div;

  function "/" ( bv1, bv2 : in bit_vector ) return bit_vector is

    variable negative_result : boolean;
    variable op1 : bit_vector(bv1'range) := bv1;
    variable op2 : bit_vector(bv2'range) := bv2;
    variable result : bit_vector(bv1'range);

  begin
    if bv1'length /= bv2'length then
      report """/"": operands of different lengths"
        severity failure;
    else
      negative_result := (op1(op1'left) = '1') xor (op2(op2'left) = '1');
      if op1(op1'left) = '1' then
        op1 := - bv1;
      end if;
      if op2(op2'left) = '1' then
        op2 := - bv2;
      end if;
      result := bv_divu(op1, op2);
      if negative_result then
        result := - result;
      end if;
    end if;
    return result;
  end function "/";

  procedure bv_divu ( bv1, bv2 : in bit_vector;
      	       	      bv_quotient : out bit_vector;
		      bv_remainder : out bit_vector;
		      div_by_zero : out boolean ) is

    constant len : natural := bv1'length;
    constant zero_divisor : bit_vector(len-1 downto 0) := (others => '0');
    alias dividend : bit_vector(bv1'length-1 downto 0) is bv1;
    variable divisor : bit_vector(bv2'length downto 0) := '0' & bv2;
    variable quotient : bit_vector(len-1 downto 0);
    variable remainder : bit_vector(len downto 0) := (others => '0');
    variable ignore_overflow  : boolean;

  begin
    if bv1'length /= bv2'length
      or bv1'length /= bv_quotient'length or bv1'length /= bv_remainder'length then
      report "bv_divu: operands of different lengths"
        severity failure;
    else
      --  check for zero divisor
      if bv2 = zero_divisor then
        div_by_zero := true;
        return;
      end if;
      --  perform division
      for iter in len-1 downto 0 loop
        if remainder(len) = '0' then
          remainder := remainder sll 1;
          remainder(0) := dividend(iter);
          bv_sub(remainder, divisor, remainder, ignore_overflow);
        else
          remainder := remainder sll 1;
          remainder(0) := dividend(iter);
          bv_add(remainder, divisor, remainder, ignore_overflow);
        end if;
        quotient(iter) := not remainder(len);
      end loop;
      if remainder(len) = '1' then
        bv_add(remainder, divisor, remainder, ignore_overflow);
      end if;
      bv_quotient := quotient;
      bv_remainder := remainder(len - 1 downto 0);
      div_by_zero := false;
    end if;
  end procedure bv_divu;

  procedure bv_divu ( bv1, bv2 : in bit_vector;
      	       	      bv_quotient : out bit_vector;
		      div_by_zero : out boolean ) is

    variable ignore_remainder : bit_vector(bv_quotient'range);

  begin
    bv_divu(bv1, bv2, bv_quotient, ignore_remainder, div_by_zero);
  end procedure bv_divu;

  function bv_divu ( bv1, bv2 : in bit_vector ) return bit_vector is

    variable result : bit_vector(bv1'range);
    variable tmp_div_by_zero : boolean;

  begin
    bv_divu(bv1, bv2, result, tmp_div_by_zero);
    return result;
  end function bv_divu;

  ----------------------------------------------------------------
  --  Arithmetic comparison operators.
  --  Perform comparisons on bit vector encoded signed integers.
  --  (For unsigned integers, built in lexical comparison does
  --  the required operation.)
  ----------------------------------------------------------------

  function bv_lt ( bv1, bv2 : in bit_vector ) return boolean is

    variable tmp1 : bit_vector(bv1'range) := bv1;
    variable tmp2 : bit_vector(bv2'range) := bv2;

  begin
    assert bv1'length = bv2'length
      report "bv_lt: operands of different lengths"
      severity failure;
    tmp1(tmp1'left) := not tmp1(tmp1'left);
    tmp2(tmp2'left) := not tmp2(tmp2'left);
    return tmp1 < tmp2;
  end function bv_lt;

  function bv_le ( bv1, bv2 : in bit_vector ) return boolean is

    variable tmp1 : bit_vector(bv1'range) := bv1;
    variable tmp2 : bit_vector(bv2'range) := bv2;

  begin
    assert bv1'length = bv2'length
      report "bv_le: operands of different lengths"
      severity failure;
    tmp1(tmp1'left) := not tmp1(tmp1'left);
    tmp2(tmp2'left) := not tmp2(tmp2'left);
    return tmp1 <= tmp2;
  end function bv_le;

  function bv_gt ( bv1, bv2 : in bit_vector ) return boolean is

    variable tmp1 : bit_vector(bv1'range) := bv1;
    variable tmp2 : bit_vector(bv2'range) := bv2;

  begin
    assert bv1'length = bv2'length
      report "bv_gt: operands of different lengths"
      severity failure;
    tmp1(tmp1'left) := not tmp1(tmp1'left);
    tmp2(tmp2'left) := not tmp2(tmp2'left);
    return tmp1 > tmp2;
  end function bv_gt;

  function bv_ge ( bv1, bv2 : in bit_vector ) return boolean is

    variable tmp1 : bit_vector(bv1'range) := bv1;
    variable tmp2 : bit_vector(bv2'range) := bv2;

  begin
    assert bv1'length = bv2'length
      report "bv_ged: operands of different lengths"
      severity failure;
    tmp1(tmp1'left) := not tmp1(tmp1'left);
    tmp2(tmp2'left) := not tmp2(tmp2'left);
    return tmp1 >= tmp2;
  end function bv_ge;

  ----------------------------------------------------------------
  --  Extension operators - convert a bit vector to a longer one
  ----------------------------------------------------------------

  function bv_sext ( bv : in bit_vector;
      	      	     length : in natural ) return bit_vector is

    alias bv_norm : bit_vector(bv'length - 1 downto 0) is bv;
    variable result : bit_vector(length - 1 downto 0) := (others => bv(bv'left));
    variable src_length : natural := bv'length;

  begin
    if src_length > length then
      src_length := length;
    end if;
    result(src_length - 1 downto 0) := bv_norm(src_length - 1 downto 0);
    return result;
  end function bv_sext;

  function bv_zext ( bv : in bit_vector;
      	      	     length : in natural ) return bit_vector is

    alias bv_norm : bit_vector(bv'length - 1 downto 0) is bv;
    variable result : bit_vector(length - 1 downto 0) := (others => '0');
    variable src_length : natural := bv'length;

  begin
    if src_length > length then
      src_length := length;
    end if;
    result(src_length - 1 downto 0) := bv_norm(src_length - 1 downto 0);
    return result;
  end function bv_zext;

end package body bv_arithmetic;
