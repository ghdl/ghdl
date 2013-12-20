
-- Copyright (C) 2002 Morgan Kaufmann Publishers, Inc

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

entity bottom is
  port ( -- . . . );
    --
      port_name : in bit := '0' );
    --
end entity bottom;

--------------------------------------------------

architecture bottom_arch of bottom is

  signal bot_sig : -- . . .;          -- 5
    --
    bit;
    --

  procedure proc ( -- . . . ) is
    --
    param_name : in bit := '0' ) is
    --
    variable v : -- . . .;            -- 6
    --
    bit;
    --
  begin
    -- . . .
    --
    report "--6: " & v'path_name;
    report "--6: " & v'instance_name;
    --
  end procedure proc;

begin

  delays : block is
    constant d : integer := 1;      -- 7
  begin
    -- . . .
    --
    assert false report "--7: " & d'path_name;
    assert false report "--7: " & d'instance_name;
    --
  end block delays;

  func : block is
  begin

    process is
      variable v : -- . . .;          -- 8
      --
      bit;
      --
    begin
      -- . . .
      --
      report "--5: " & bot_sig'path_name;
      report "--5: " & bot_sig'instance_name;
      report "--8: " & v'path_name;
      report "--8: " & v'instance_name;
      proc(param_name => open);
      wait;
      --
      --
    end process;

  end block func;

end architecture bottom_arch;
