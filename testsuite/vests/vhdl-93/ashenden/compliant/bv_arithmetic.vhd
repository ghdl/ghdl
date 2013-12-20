
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
-- $Id: bv_arithmetic.vhd,v 1.2 2001-10-25 01:24:24 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

--------------------------------------------------------------------------
--
--  Bit-vector arithmetic package interface.
--  
--  Does arithmetic and logical operations on bit vectors, treating them
--  as either unsigned or signed (two's complement) integers.  Leftmost bit
--  is most-significant or sign bit, rightmost bit is least-significant
--  bit.  Dyadic operations need the two arguments to be of the same
--  length; however, their index ranges and directions may differ.  Results
--  must be of the same length as the operands.
--  
--------------------------------------------------------------------------

package bv_arithmetic is

  function bv_to_natural ( bv : in bit_vector ) return natural;

  function natural_to_bv ( nat : in natural;
      	      	      	   length : in natural ) return bit_vector;

  function bv_to_integer ( bv : in bit_vector ) return integer;

  function integer_to_bv ( int : in integer;
      	      	      	   length : in natural ) return bit_vector;

  procedure bv_add ( bv1, bv2 : in bit_vector;
      	       	     bv_result : out bit_vector;
		     overflow : out boolean );

  function "+" ( bv1, bv2 : in bit_vector ) return bit_vector;

  procedure bv_sub ( bv1, bv2 : in bit_vector;
      	       	     bv_result : out bit_vector;
		     overflow : out boolean );

  function "-" ( bv1, bv2 : in bit_vector ) return bit_vector;

  procedure bv_addu ( bv1, bv2 : in bit_vector;
      	       	      bv_result : out bit_vector;
		      overflow : out boolean );

  function bv_addu ( bv1, bv2 : in bit_vector ) return bit_vector;

  procedure bv_subu ( bv1, bv2 : in bit_vector;
      	       	      bv_result : out bit_vector;
		      overflow : out boolean );

  function bv_subu ( bv1, bv2 : in bit_vector ) return bit_vector;

  procedure bv_neg ( bv : in bit_vector;
                     bv_result : out bit_vector;
                     overflow : out boolean );

  function "-" ( bv : in bit_vector ) return bit_vector;

  procedure bv_mult ( bv1, bv2 : in bit_vector;
      	       	      bv_result : out bit_vector;
		      overflow : out boolean );

  function "*" ( bv1, bv2 : in bit_vector ) return bit_vector;

  procedure bv_multu ( bv1, bv2 : in bit_vector;
      	       	       bv_result : out bit_vector;
		       overflow : out boolean );

  function bv_multu ( bv1, bv2 : in bit_vector ) return bit_vector;

  procedure bv_div ( bv1, bv2 : in bit_vector;
      	       	     bv_result : out bit_vector;
		     div_by_zero : out boolean;
                     overflow : out boolean );

  function "/" ( bv1, bv2 : in bit_vector ) return bit_vector;

  procedure bv_divu ( bv1, bv2 : in bit_vector;
      	       	      bv_quotient : out bit_vector;
		      bv_remainder : out bit_vector;
		      div_by_zero : out boolean );

  procedure bv_divu ( bv1, bv2 : in bit_vector;
      	       	      bv_quotient : out bit_vector;
		      div_by_zero : out boolean );

  function bv_divu ( bv1, bv2 : in bit_vector )  return bit_vector;

  function bv_lt ( bv1, bv2 : in bit_vector ) return boolean;

  function bv_le ( bv1, bv2 : in bit_vector ) return boolean;

  function bv_gt ( bv1, bv2 : in bit_vector ) return boolean;

  function bv_ge ( bv1, bv2 : in bit_vector ) return boolean;

  function bv_sext ( bv : in bit_vector;
      	      	     length : in natural ) return bit_vector;

  function bv_zext ( bv : in bit_vector;
      	      	     length : in natural ) return bit_vector;

end package bv_arithmetic;
