-- --------------------------------------------------------------------
--
-- Copyright © 2008 by IEEE. All rights reserved.
--
-- This source file is an essential part of IEEE Std 1076-2008,
-- IEEE Standard VHDL Language Reference Manual. This source file may not be
-- copied, sold, or included with software that is sold without written 
-- permission from the IEEE Standards Department. This source file may be 
-- copied for individual use between licensed users. This source file is
-- provided on an AS IS basis. The IEEE disclaims ANY WARRANTY EXPRESS OR
-- IMPLIED INCLUDING ANY WARRANTY OF MERCHANTABILITY AND FITNESS FOR USE
-- FOR A PARTICULAR PURPOSE. The user of the source file shall indemnify
-- and hold IEEE harmless from any damages or liability arising out of the
-- use thereof.
--
--   Title     :  Floating-point package (Instantiated package declaration)
--             :
--   Library   :  This package shall be compiled into a library
--             :  symbolically named IEEE.
--             :
--   Developers:  Accellera VHDL-TC and IEEE P1076 Working Group
--             :
--   Purpose   :  This packages defines basic binary floating point
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

library ieee;

package float_pkg is new IEEE.float_generic_pkg
  generic map (
    float_exponent_width => 8,    -- float32'high
    float_fraction_width => 23,   -- -float32'low
    float_round_style    => IEEE.fixed_float_types.round_nearest,  -- round nearest algorithm
    float_denormalize    => true,  -- Use IEEE extended floating
    float_check_error    => true,  -- Turn on NAN and overflow processing
    float_guard_bits     => 3,     -- number of guard bits
    no_warning           => false, -- show warnings
    fixed_pkg            => IEEE.fixed_pkg
    );
