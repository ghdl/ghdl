--  Iir to ortho translator.
--  Copyright (C) 2002 - 2014 Tristan Gingold
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

package Trans.Helpers2 is
   --  Copy a fat pointer.
   --  D and S are stabilized fat pointers.
   procedure Copy_Fat_Pointer (D : Mnode; S: Mnode);

   --  Create a constant (of name ID) for string STR.
   --  Append a NUL terminator (to make interfaces with C easier).
   function Create_String (Str : String; Id : O_Ident) return O_Dnode;

   function Create_String (Str : String; Id : O_Ident; Storage : O_Storage)
                              return O_Dnode;

   function Create_String (Str : Name_Id; Id : O_Ident; Storage : O_Storage)
                              return O_Dnode;

   function Create_String_Len (Str : String; Id : O_Ident) return O_Cnode;

   procedure Gen_Memcpy (Dest : O_Enode; Src : O_Enode; Length : O_Enode);

   --  Allocate SIZE bytes aligned on the biggest alignment and return a
   --  pointer of type PTYPE.
   function Gen_Alloc
     (Kind : Allocation_Kind; Size : O_Enode; Ptype : O_Tnode)
         return O_Enode;

   --  Allocate on the heap LENGTH bytes aligned on the biggest alignment,
   --  and returns a pointer of type PTYPE.
   --function Gen_Malloc (Length : O_Enode; Ptype : O_Tnode) return O_Enode;

   --  Call a procedure (DATA_TYPE) for each signal of TARG.
   procedure Register_Signal
     (Targ : Mnode; Targ_Type : Iir; Proc : O_Dnode);

   --  Call PROC for each scalar signal of list LIST.
   procedure Register_Signal_List (List : Iir_List; Proc : O_Dnode);

   --  Often used subprograms for Foreach_non_composite
   --  when DATA_TYPE is o_enode.
   function Gen_Oenode_Prepare_Data_Composite
     (Targ: Mnode; Targ_Type : Iir; Val : O_Enode)
         return Mnode;
   function Gen_Oenode_Update_Data_Array (Val       : Mnode;
                                          Targ_Type : Iir;
                                          Index     : O_Dnode)
                                             return O_Enode;
   function Gen_Oenode_Update_Data_Record
     (Val : Mnode; Targ_Type : Iir; El : Iir_Element_Declaration)
         return O_Enode;
   procedure Gen_Oenode_Finish_Data_Composite (Data : in out Mnode);

   --  Just the line from TARGET location.
   function Get_Line_Number (Target: Iir) return Natural;

   procedure Assoc_Filename_Line (Assoc : in out O_Assoc_List;
                                  Line  : Natural);
end Trans.Helpers2;
