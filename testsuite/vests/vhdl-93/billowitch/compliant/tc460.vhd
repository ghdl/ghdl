
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
-- $Id: tc460.vhd,v 1.2 2001-10-26 16:29:54 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY model IS
  PORT
    (
      F1:  OUT     integer := 3;
      F2:  INOUT    integer := 3;
      F3:  IN    integer
      );
END model;

architecture model of model is
begin
  process
  begin
    wait for 1 ns;
    assert F3= 3
      report"wrong initialization of F3 through type conversion" severity failure;
    assert F2 = 3
      report"wrong initialization of F2 through type conversion" severity failure;
    wait;
  end process;
end;


ENTITY c03s02b01x01p19n01i00460ent IS
END c03s02b01x01p19n01i00460ent;

ARCHITECTURE c03s02b01x01p19n01i00460arch OF c03s02b01x01p19n01i00460ent IS

  type       four_value    is ('Z','0','1','X');
  subtype    binary       is four_value range '0' to '1';
  subtype    word       is bit_vector(0 to  15);
  constant    size       :integer := 7;
  type       primary_memory    is array(0 to size) of word;

  type primary_memory_module is   
    record   
      enable      :binary;
      memory_number   :primary_memory;
    end record;   

  type       whole_memory is array(0 to size) of primary_memory_module;

  constant    C61 : word       := (others =>'0' );
  constant    C64 : primary_memory    := (others => C61);
  constant    C65 : primary_memory_module := ('1',C64);
  constant    C66 : whole_memory    := (others => C65);

  function complex_scalar(s : whole_memory) return integer is
  begin
    return 3;
  end complex_scalar;
  function scalar_complex(s : integer) return whole_memory is
  begin
    return C66;
  end scalar_complex;
  component model1
    PORT
      (    
        F1:  OUT     integer;
        F2:  INOUT    integer;
        F3:  IN    integer
        );
  end component;
  for T1 : model1 use entity work.model(model);

  signal S1 : whole_memory;
  signal S2 : whole_memory;
  signal S3 : whole_memory:= C66;
BEGIN
  T1: model1
    port map (
      scalar_complex(F1) => S1,
      scalar_complex(F2) => complex_scalar(S2),
      F3 => complex_scalar(S3)
      );
  TESTING: PROCESS
  BEGIN
    wait for 1 ns;
    assert NOT((S1 = C66) and (S2 = C66)) 
      report "***PASSED TEST: c03s02b01x01p19n01i00460" 
      severity NOTE;
    assert ((S1 = C66) and (S2 = C66)) 
      report "***FAILED TEST: c03s02b01x01p19n01i00460 - For an interface object of mode out, buffer, inout, or linkage, if the formal part includes a type conversion function, then the parameter subtype of that function must be a constrained array subtype."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s02b01x01p19n01i00460arch;
