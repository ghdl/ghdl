
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

package inline_09_defs is

  attribute attr : integer;

end package inline_09_defs;



use work.inline_09_defs.all;

entity e is
  port ( p : in bit );
  attribute attr of p : signal is 1;
end entity e;


architecture arch of e is
begin

  assert false report integer'image(p'attr);

end architecture arch;



use work.inline_09_defs.all;

entity inline_09 is
end entity inline_09;



architecture test of inline_09 is

  signal s : bit;

  attribute attr of s : signal is 2;

begin

  -- code from book

  c1 : entity work.e(arch)
    port map ( p => s );

  -- end code from book

end architecture test;
