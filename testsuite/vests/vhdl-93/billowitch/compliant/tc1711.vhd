
-- Copyright (C) 2001 Bill Billowitch.

-- Some of the work to develop this test suite was done with Air Force
-- support.  The Air Force and Bill Billowitch assume no
-- responsibilities for this software.

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
-- $Id: tc1711.vhd,v 1.2 2001-10-26 16:29:43 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c09s02b00x00p10n01i01711pkg is

  -- Type declarations.
  type    SWITCH_LEVEL   is ( '0', '1', 'X' );
  type    S_logic_vector is array(positive range <>) of SWITCH_LEVEL;

  -- Define the bus resolution function.
  function switchf( s : S_logic_vector ) return SWITCH_LEVEL;

  -- Further type declarations.
  subtype SWITCH_T       is switchF SWITCH_LEVEL;
--   type    WORD           is array(0 to 31) of SWITCH_T;
  type    WORD           is array(1 to 32) of SWITCH_T;

end c09s02b00x00p10n01i01711pkg;

package body  c09s02b00x00p10n01i01711pkg is

  function switchf( s : S_logic_vector ) return SWITCH_LEVEL is
  begin
    return( S(1) );
  end switchf;

end c09s02b00x00p10n01i01711pkg;


ENTITY c09s02b00x00p10n01i01711ent IS
  generic ( GenFive : in INTEGER := 12 );
END c09s02b00x00p10n01i01711ent;
use work.c09s02b00x00p10n01i01711pkg.all;
ARCHITECTURE c09s02b00x00p10n01i01711arch OF c09s02b00x00p10n01i01711ent IS
  -- Local constants.
  constant  Three : integer := 3; 

  -- Local signals.
  signal A       : WORD;

BEGIN
  -- Test signal arrays indexed using a generic constants.  (locally static)
  TESTING: PROCESS(A(GenFive))
    -- Local variables.
    variable INITED : BOOLEAN := FALSE;
    variable NewTime: TIME;
  BEGIN
    -- Perform the first piece of assignments.
    if  (not(INITED)) then
      INITED       := TRUE;
      A( GenFive ) <= 'X' after 10 ns;
      NewTime    := NOW + 10 ns;
    end if;
    if (now = NewTime) then
      assert NOT( A(GenFive) = 'X' )
        report "***PASSED TEST: c09s02b00x00p10n01i01711"
        severity NOTE;
      assert ( A(GenFive) = 'X' )
        report "***FAILED TEST: c09s02b00x00p10n01i01711 - Signal arrays indexed using a generic constants may be used in the sentitivity list of a porcess statement."
        severity ERROR;
    end if;
  END PROCESS TESTING;

END c09s02b00x00p10n01i01711arch;
