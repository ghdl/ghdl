--  Verilog expressions interpreter
--  Copyright (C) 2023 Tristan Gingold
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

with Types; use Types;
with Verilog.Types; use Verilog.Types;
with Verilog.Allocates; use Verilog.Allocates;
with Verilog.Nodes; use Verilog.Nodes;
with Verilog.Storages; use Verilog.Storages;
with Verilog.Sv_Arrays;
with Verilog.Sv_Classes;

package Verilog.Executions is
   procedure Execute_Expression
     (Frame : Frame_Ptr; Dest : Data_Ptr; Expr : Node);

   procedure Prepare_Call (Outer_Frame : Frame_Ptr;
                           Call : Node;
                           Subprg : Node;
                           Handle : Sv_Classes.Sv_Class_Handle;
                           Link : out Frame_Link_Type);

   procedure Execute_Expression_Int32 (Frame : Frame_Ptr;
                                       Expr : Node;
                                       Val : out Int32;
                                       Is_Undef : out Boolean);

   --  Return True iff NAME denotes a name that is a vector or a part of a
   --  vector.  This is needed because bit/logic representation is different
   --  when standalone or when part of a vector.
   function Is_Vector_Name (Name : Node; Ntype : Node) return Boolean;

   procedure Finalize_Data (Dest : Data_Ptr; Atype : Node);

   procedure Execute_Declarations (Frame : Frame_Ptr; Decls : Node);
   procedure Finalize_Declarations (Frame : Frame_Ptr; Decls : Node);

   --  Delimit NAME for a vector type.
   --  VALUE is the pointer to the start of the vector.
   --  NOFFSET and WIDTH specifies the elements to be modified.  They always
   --  must be in the vector (so NOFFSET + WIDTH <= vector length).
   --  WIDTH can be 0 if NAME is outside the vector (bad index).
   procedure Execute_Name_Vector (Frame : Frame_Ptr;
                                  Name : Node;
                                  Force : Boolean;
                                  Value : out Data_Ptr;
                                  Noffset : out Bit_Offset;
                                  Doffset : out Bit_Offset;
                                  Width : out Width_Type;
                                  Update : out Update_Acc);

   procedure Execute_Name_Nonvec (Frame : Frame_Ptr;
                                  Name : Node;
                                  Force : Boolean;
                                  Value : out Data_Ptr;
                                  Update : out Update_Acc);

   function Execute_Condition (Val : Data_Ptr; Expr : Node)
                              return Tri_State_Type;
   function Execute_Condition (Frame : Frame_Ptr; Expr : Node)
                              return Tri_State_Type;

   procedure Execute_Unary_Expression
     (Expr : Node; Op : Data_Ptr; Res : Data_Ptr);
   procedure Execute_Binary_Expression
     (Expr : Node; Left : Data_Ptr; Right : Data_Ptr; Dest : Data_Ptr);
   procedure Execute_String_Literal (Dest : Data_Ptr; Expr : Node);
   procedure Execute_Real_Number (Dest : Data_Ptr; Expr : Node);

   procedure Execute_Assign_Operator (Frame : Frame_Ptr; Stmt : Node);

   --  Allocate a dynamic array of SIZE.
   function Create_Dynamic_Array (Arr_Type : Node; Size : Int32)
                                 return Sv_Arrays.Sv_Dyn_Array_Ptr;

   --  Copy SRC to DEST.  DEST must be standalone (not a part of a vector).
   --  (Used by Sv_Maps).
   procedure Execute_Simple_Copy
     (Dest : Data_Ptr; Src : Data_Ptr; Etype : Node);

end Verilog.Executions;
