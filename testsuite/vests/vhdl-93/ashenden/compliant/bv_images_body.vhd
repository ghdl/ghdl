
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
-- $Id: bv_images_body.vhd,v 1.3 2001-10-26 16:29:33 paw Exp $
-- $Revision: 1.3 $
--
-- ---------------------------------------------------------------------

--------------------------------------------------------------------------
--
--  bv_images package body.
--
--  Functions that return the string image of values.
--  Each image is a correctly formed literal according to the
--  rules of VHDL-93.
--
--------------------------------------------------------------------------

package body bv_images is


  -- Image of bit vector as binary bit string literal
  -- (in the format B"...")
  -- Length of result is bv'length + 3

  function image (bv : in bit_vector) return string is

    alias bv_norm : bit_vector(1 to bv'length) is bv;
    variable result : string(1 to bv'length + 3);

  begin
    result(1) := 'B';
    result(2) := '"';
    for index in bv_norm'range loop
      if bv_norm(index) = '0' then
        result(index + 2) := '0';
      else
        result(index + 2) := '1';
      end if;
    end loop;
    result(bv'length + 3) := '"';
    return result;
  end image;

  ----------------------------------------------------------------

  -- Image of bit vector as octal bit string literal
  -- (in the format O"...")
  -- Length of result is (bv'length+2)/3 + 3

  function image_octal (bv : in bit_vector) return string is

    constant nr_digits : natural := (bv'length + 2) / 3;
    variable result : string(1 to nr_digits + 3);
    variable bits : bit_vector(0 to 3*nr_digits - 1) := (others => '0');
    variable three_bits : bit_vector(0 to 2);
    variable digit : character;

  begin
    result(1) := 'O';
    result(2) := '"';
    bits(bits'right - bv'length + 1 to bits'right) := bv;
    for index in 0 to nr_digits - 1 loop
      three_bits := bits(3*index to 3*index + 2);
      case three_bits is
	when b"000" =>
	  digit := '0';
	when b"001" =>
	  digit := '1';
	when b"010" =>
	  digit := '2';
	when b"011" =>
	  digit := '3';
	when b"100" =>
	  digit := '4';
	when b"101" =>
	  digit := '5';
	when b"110" =>
	  digit := '6';
	when b"111" =>
	  digit := '7';
      end case;
      result(index + 3) := digit;
    end loop;
    result(nr_digits + 3) := '"';
    return result;
  end image_octal;

  ----------------------------------------------------------------

  -- Image of bit vector as hex bit string literal
  -- (in the format X"...")
  -- Length of result is (bv'length+3)/4 + 3

  function image_hex (bv : in bit_vector) return string is

    constant nr_digits : natural := (bv'length + 3) / 4;
    variable result : string(1 to nr_digits + 3);
    variable bits : bit_vector(0 to 4*nr_digits - 1) := (others => '0');
    variable four_bits : bit_vector(0 to 3);
    variable digit : character;

  begin
    result(1) := 'X';
    result(2) := '"';
    bits(bits'right - bv'length + 1 to bits'right) := bv;
    for index in 0 to nr_digits - 1 loop
      four_bits := bits(4*index to 4*index + 3);
      case four_bits is
	when b"0000" =>
	  digit := '0';
	when b"0001" =>
	  digit := '1';
	when b"0010" =>
	  digit := '2';
	when b"0011" =>
	  digit := '3';
	when b"0100" =>
	  digit := '4';
	when b"0101" =>
	  digit := '5';
	when b"0110" =>
	  digit := '6';
	when b"0111" =>
	  digit := '7';
	when b"1000" =>
	  digit := '8';
	when b"1001" =>
	  digit := '9';
	when b"1010" =>
	  digit := 'A';
	when b"1011" =>
	  digit := 'B';
	when b"1100" =>
	  digit := 'C';
	when b"1101" =>
	  digit := 'D';
	when b"1110" =>
	  digit := 'E';
	when b"1111" =>
	  digit := 'F';
      end case;
      result(index + 3) := digit;
    end loop;
    result(nr_digits + 3) := '"';
    return result;
  end image_hex;


end bv_images;
