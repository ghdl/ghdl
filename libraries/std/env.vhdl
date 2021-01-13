--  Std.Env package declaration.  This file is part of GHDL.
--  This file was written from the clause 14.3 of the VHDL LRM.
--  Copyright (C) 2014 Tristan Gingold
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 2 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <gnu.org/licenses>.

package Env is
  procedure Stop (Status : Integer);
  procedure Stop;

  procedure Finish (status : Integer);
  procedure Finish;

  function Resolution_Limit return Delay_Length;
end package Env;
