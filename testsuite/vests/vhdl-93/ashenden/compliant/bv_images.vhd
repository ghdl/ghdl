
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
-- $Id: bv_images.vhd,v 1.2 2001-10-25 01:24:24 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

--------------------------------------------------------------------------
--
--  bv_images package specification.
--
--  Functions that return the string image of values.
--  Each image is a correctly formed literal according to the
--  rules of VHDL-93.
--
--------------------------------------------------------------------------

package bv_images is

  -- Image of bit vector as binary bit string literal
  -- (in the format B"...")
  -- Length of result is bv'length + 3

  function image (bv : in bit_vector) return string;

  -- Image of bit vector as octal bit string literal
  -- (in the format O"...")
  -- Length of result is (bv'length+2)/3 + 3

  function image_octal (bv : in bit_vector) return string;

  -- Image of bit vector as hex bit string literal
  -- (in the format X"...")
  -- Length of result is (bv'length+3)/4 + 3

  function image_hex (bv : in bit_vector) return string;

end bv_images;

