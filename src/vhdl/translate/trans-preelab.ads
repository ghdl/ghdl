--  Build ortho structures from iir_values
--  Copyright (C) 2016 Tristan Gingold
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

package Trans.Preelab is
   --  Primary unit + secondary unit (architecture name which may be null)
   --  to elaborate.
   procedure Pre_Elaborate (Primary : String;
                            Secondary : String;
                            Filelist : String;
                            Whole : Boolean);
end Trans.Preelab;
