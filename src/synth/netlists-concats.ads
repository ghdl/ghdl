--  Provide a simple way to concat an unknown number of nets.
--  Copyright (C) 2019 Tristan Gingold
--
--  This file is part of GHDL.
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; if not, write to the Free Software
--  Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston,
--  MA 02110-1301, USA.

with Dyn_Tables;
with Netlists.Builders; use Netlists.Builders;

package Netlists.Concats is
   type Concat_Type is limited private;

   --  Append net N to C.
   procedure Append (C : in out Concat_Type; N : Net);

   --  Get the concatenation of all nets in C.  Reset C.
   procedure Build (Ctxt : Context_Acc; C : in out Concat_Type; N : out Net);
private
   Static_Last : constant Int32 := 16;

   package Net_Tables is new Dyn_Tables
     (Table_Component_Type => Net,
      Table_Index_Type => Int32,
      Table_Low_Bound => 1,
      Table_Initial => Integer (Static_Last * 2));

   type Concat_Type is limited record
      Len : Int32 := 0;
      Sarr : Net_Tables.Table_Type (1 .. Static_Last);
      Darr : Net_Tables.Instance;
   end record;
end Netlists.Concats;
