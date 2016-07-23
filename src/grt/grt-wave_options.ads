--  GHDL Run Time (GRT) - mono-thread version.
--  Copyright (C) 2005 - 2014 Tristan Gingold
--
--  GHDL is free software; you can redistribute it and/or modify it under
--  the terms of the GNU General Public License as published by the Free
--  Software Foundation; either version 2, or (at your option) any later
--  version.
--
--  GHDL is distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
--  for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with GCC; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.

package Grt.Wave_Options is
   pragma Preelaborate;

   type String_Acc is access String;
   type String_Cst is access constant String;
   Value_String_Size : constant := 10;

   File_Path : String_Cst;

   type Line_Context_Type is record
      Str : String_Acc;
      Num : Natural;
      Max_Level : Natural;
   end record;
   type Line_Context_Acc is access Line_Context_Type;

   type Elem_Kind_Type is (Not_Found, Pkg_Entity, Signal);
   type Elem_Type;
   type Elem_Acc is access Elem_Type;
   type Elem_Type is record
      Name : String_Cst;
      Line_Context : Line_Context_Acc;
      Kind : Elem_Kind_Type;
      Next_Sibling : Elem_Acc;
      Next_Child : Elem_Acc;
   end record;

   type Tree_Index_Type is (Pkg, Entity);
   type Tree_Array is array (Tree_Index_Type) of Elem_Acc;
   Trees : Tree_Array := (others => null);
   type Sep_Array is array (Tree_Index_Type) of Character;
   Seps : constant Sep_Array := (Pkg => '.', Entity => '/');

   type Severity_Type is (Error, Warning);

   procedure Print_Context
     (Line_Context : Line_Context_Acc; Severity : Severity_Type);
   procedure Error_Context (Msg : String;
                            Line_Context : Line_Context_Acc;
                            Severity : Severity_Type := Error);

end Grt.Wave_Options;
