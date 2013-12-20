
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
-- $Id: tc1655.vhd,v 1.2 2001-10-26 16:29:42 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c09s00b00x00p05n01i01655pkg is
  procedure cpc (constant loc : string);
end c09s00b00x00p05n01i01655pkg;

package body c09s00b00x00p05n01i01655pkg is
  procedure cpc (constant loc : string) is      -- concurrent procedure
  begin
    assert false
      report "Concurrent procedure called from " & loc            
      severity note ;
  end cpc;
end c09s00b00x00p05n01i01655pkg;

use work.c09s00b00x00p05n01i01655pkg.all;

entity c09s00b00x00p05n01i01655ent_a is
  port (signal pi : in  bit;
        signal po : out bit
        );
begin
  cas : assert false
    report "Labeled concurrent assert called from component."
    severity note ;

  cpcc : cpc("component entity");

  ppsc :                                -- passive process stmt
  process (pi)
  begin
    assert false
      report "Passive process can be labeled in component."
      severity note ;
  end process;
end c09s00b00x00p05n01i01655ent_a;

architecture c09s00b00x00p05n01i01655arch_a of c09s00b00x00p05n01i01655ent_a is
begin
  cpc("component architecture");
end;

use work.c09s00b00x00p05n01i01655pkg.all, work.c09s00b00x00p05n01i01655ent_a;

ENTITY c09s00b00x00p05n01i01655ent IS
  port (signal pi : in  bit;
        signal po : out bit
        );
begin
  cas : assert false
    report "Labeled concurrent assert called from entity."
    severity note ;
  
  cpce : cpc("entity.");
  
  ppse :                                -- passive process stmt
  process (pi)
  begin
    assert false
      report "Passive process can be labeled in entity."
      severity note ;
  end process;
END c09s00b00x00p05n01i01655ent;

ARCHITECTURE c09s00b00x00p05n01i01655arch OF c09s00b00x00p05n01i01655ent IS
  signal lab_sig : boolean := true;
  
  component comp
    port (signal pi : in bit;
          signal po : out bit
          );
  end component; -- comp
  for lcia : comp use entity work.c09s00b00x00p05n01i01655ent_a(c09s00b00x00p05n01i01655arch_a)
    port map (pi, po);
BEGIN
  casa : assert false
    report "Labeled concurrent assert called from architecture."
    severity note ;
  
  cpca : cpc("architecture.");
  
  ppsa : process (pi)
  begin
    assert false
      report "Passive process can be labeled in architecture."
      severity note ;
  end process;
  
  lba: block
  begin
    cpcb : cpc("block.");
    
    casb : assert false
      report "Labeled concurrent assert called from labeled block."
      severity note ;
  end block lba;
  
  csa : lab_sig <= false;
  assert lab_sig
    report "Labeled concurrent signal assignment executed in architecture."
    severity note ;
  
  lcia : comp
    port map (pi => pi, po => po);

  TESTING: PROCESS
  BEGIN
    assert FALSE 
      report "***PASSED TEST: c09s00b00x00p05n01i01655 - This test need manual check to the ASSERTION statement."
      severity NOTE;
    wait;
  END PROCESS TESTING;

END c09s00b00x00p05n01i01655arch;
