
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
-- $Id: ch_01_tb_01_03.vhd,v 1.3 2001-10-26 16:29:33 paw Exp $
-- $Revision: 1.3 $
--
-- ---------------------------------------------------------------------

entity shift_adder is
  port ( addend : in integer;  augend : in integer;
  sum : out integer;
  add_control : in bit );
end entity shift_adder;

architecture behavior of shift_adder is
begin
end architecture behavior;

------------------------------------------------------------------------

entity reg is
  port ( d : in integer;  q : out integer;
  en : in bit;  reset : in bit );
end entity reg;

architecture behavior of reg is
begin
end architecture behavior;

------------------------------------------------------------------------

entity shift_reg is
  port ( d : in integer;  q : out bit;
  load : in bit;  clk : in bit );
end entity shift_reg;

architecture behavior of shift_reg is
begin
end architecture behavior;
