--  Verilog semantic analyzer (simulation)
--  Copyright (C) 2023 Tristan Gingold
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
--  along with this program.  If not, see <gnu.org/licenses>.

with System; use System;
with Ada.Unchecked_Deallocation;
with Simple_IO; use Simple_IO;
with Utils_IO; use Utils_IO;

with Verilog.Nutils; use Verilog.Nutils;
with Verilog.Errors; use Verilog.Errors;
with Verilog.Bignums; use Verilog.Bignums;
with Verilog.Sem_Utils; use Verilog.Sem_Utils;
with Verilog.Sv_Strings; use Verilog.Sv_Strings;
with Verilog.Sv_Classes; use Verilog.Sv_Classes;
with Verilog.Sv_Arrays; use Verilog.Sv_Arrays;
with Verilog.Sv_Queues; use Verilog.Sv_Queues;
with Verilog.Sv_Maps; use Verilog.Sv_Maps;
with Verilog.Elaborate;
with Verilog.Executions; use Verilog.Executions;
with Verilog.Vpi;
with Verilog.Debugger;
with Verilog.Disp_Verilog;
with Files_Map;

package body Verilog.Simulation is
   procedure Execute_Simple_Statement (Frame : Frame_Ptr; Insn : Node);

   --  Queue of processes to execute.
   type Procs_Queue is record
      First, Last : Process_Acc;
   end record;

   procedure Free is new Ada.Unchecked_Deallocation
     (Process_Type, Process_Acc);

   type Event_Kind is (Nba_Vector_Event, Nba_Nonvec_Event, Update_Event);

   type Event_Type;
   type Event_Acc is access Event_Type;
   type Event_Type (Kind : Event_Kind; Size : Storage_Index) is record
      Next : Event_Acc;
      case Kind is
         when Nba_Vector_Event
           | Nba_Nonvec_Event
           | Update_Event =>
            Target_Data : Data_Ptr;
            Target_Type : Node;
            Target_Offset : Bit_Offset;
            Target_Doffset : Bit_Offset;
            Target_Width : Width_Type;
            Target_Update : Update_Acc;
            Value : Storage_Type (1 .. Size);
      end case;
   end record;

   type Event_Queue is record
      First, Last : Event_Acc;
   end record;

   procedure Free is new Ada.Unchecked_Deallocation
     (Event_Type, Event_Acc);

   type Time_Wheel_Entry is record
      Inactive : Procs_Queue;
      Nba : Event_Queue;
   end record;
   type Time_Wheel_Entry_Acc is access all Time_Wheel_Entry;

   Time_Wheel_Size : constant Uns32 := 128;
   type Time_Wheel_Type is array (Uns32 range 0 .. Time_Wheel_Size - 1)
     of aliased Time_Wheel_Entry;

   --  Optimize: use a bitmap to quickly find the next non-empty entry.
   Time_Wheel : Time_Wheel_Type;
   Time_Wheel2 : Time_Wheel_Type;
   Time_Wheel3 : Time_Wheel_Entry;
   Current_Entry : Time_Wheel_Entry_Acc;
   Time : Uns32;

   --  Append PROC on QUEUE.
   procedure Append_Queue (Queue : in out Procs_Queue; Proc : Process_Acc) is
   begin
      pragma Assert (Proc.Enabled_Chain = null);
      if Queue.First = null then
         Queue.First := Proc;
      else
         Queue.Last.Enabled_Chain := Proc;
      end if;
      Queue.Last := Proc;
   end Append_Queue;

   procedure Append (Queue : in out Procs_Queue; Proc : Process_Acc) is
   begin
      if Proc.Is_Scheduled then
         return;
      else
         Proc.Is_Scheduled := True;
      end if;

      if False then
         Put_Line ("append process at "
                     & Files_Map.Image (Get_Location (Proc.Stmt)));
      end if;

      Append_Queue (Queue, Proc);
   end Append;

   procedure Append (Queue : in out Event_Queue; Event : Event_Acc) is
   begin
      pragma Assert (Event.Next = null);
      if Queue.First = null then
         Queue.First := Event;
      else
         Queue.Last.Next := Event;
      end if;
      Queue.Last := Event;
   end Append;

   procedure Extract_All (Queue : in out Event_Queue; First : out Event_Acc) is
   begin
      First := Queue.First;
      Queue := (null, null);
   end Extract_All;

   function Is_Empty (Queue : Event_Queue) return Boolean is
   begin
      return Queue.First = null;
   end Is_Empty;

   procedure Extract_All (Queue : in out Procs_Queue; First : out Process_Acc)
   is
   begin
      First := Queue.First;
      Queue := (null, null);
   end Extract_All;

   function Is_Empty (Queue : Procs_Queue) return Boolean is
   begin
      return Queue.First = null;
   end Is_Empty;

   --  Queue of enabled processes.
   Active_Procs : Procs_Queue;

   --  Enable a process.
   procedure Activate_Process (Proc : Process_Acc) is
   begin
      Append (Active_Procs, Proc);
   end Activate_Process;

   procedure Trace_Head is
   begin
      Put ("trace: ");
   end Trace_Head;

   procedure Trace_Newline is
   begin
      New_Line;
   end Trace_Newline;

   procedure Trace (S : String) is
   begin
      Put (S);
   end Trace;

   procedure Trace_Cond (Cond : Tri_State_Type) is
   begin
      if Cond = True then
         Trace (" TRUE");
      else
         Trace (" FALSE");
      end if;
   end Trace_Cond;

   procedure Activate_Sensitized_Processes (Head : Update_El_Acc);

   procedure Activate_Edge_Process (Edge : Edge_Process_Acc)
   is
      Value : Logic_Type;
      D : Boolean;
   begin
      Value := To_Logic_Ptr (Get_Var_Data (null, Edge.Expr)).all;
      if Value = Edge.Prev then
         return;
      end if;

      case Edge.Kind is
         when N_Posedge =>
            --  Posedge: if jumps to 1 or leaves 0.
            D := Value = V_1 or Edge.Prev = V_0;
         when N_Negedge =>
            --  Negedge: if jumps to 0 or leaves 1.
            D := Value = V_0 or Edge.Prev = V_1;
      end case;
      Edge.Prev := Value;

      if D then
         Activate_Sensitized_Processes (Edge.Updates);
      end if;
   end Activate_Edge_Process;

   procedure Activate_Sensitized_Processes (Head : Update_El_Acc)
   is
      El : Update_El_Acc;
   begin
      El := Head;
      while El /= null loop
         case El.Kind is
            when Update_Edge =>
               Activate_Edge_Process (El.Edge);
            when Update_Process =>
               Activate_Process (El.Proc);
            when Update_Vpi =>
               Vpi.Execute_Cb (El.Cb);
         end case;
         El := El.Next;
      end loop;
   end Activate_Sensitized_Processes;

   procedure Activate_Sensitized_Processes (Update : Update_Acc) is
   begin
      if Update = null then
         return;
      end if;

      Activate_Sensitized_Processes (Update.List);
   end Activate_Sensitized_Processes;

   function Is_Eq (Left, Right : Data_Ptr; Etype : Node) return Boolean is
   begin
      case Get_Kind (Etype) is
         when N_Log_Packed_Array_Cst =>
            declare
               W : constant Width_Type := Get_Type_Width (Etype);
            begin
               return Is_Eq (To_Logvec_Ptr (Left), To_Logvec_Ptr (Right), W);
            end;
         when others =>
            Error_Kind ("is_eq", Etype);
      end case;
   end Is_Eq;

   function Is_Eqx (Left, Right : Data_Ptr; Etype : Node) return Boolean is
   begin
      case Get_Kind (Etype) is
         when N_Log_Packed_Array_Cst =>
            declare
               W : constant Width_Type := Get_Type_Width (Etype);
            begin
               return Is_Eqx (To_Logvec_Ptr (Left), To_Logvec_Ptr (Right), W);
            end;
         when others =>
            Error_Kind ("is_eqx", Etype);
      end case;
   end Is_Eqx;

   function Is_Eqz (Left, Right : Data_Ptr; Etype : Node) return Boolean is
   begin
      case Get_Kind (Etype) is
         when N_Log_Packed_Array_Cst =>
            declare
               W : constant Width_Type := Get_Type_Width (Etype);
            begin
               return Is_Eqz (To_Logvec_Ptr (Left), To_Logvec_Ptr (Right), W);
            end;
         when others =>
            Error_Kind ("is_eqz", Etype);
      end case;
   end Is_Eqz;

   procedure Assign_Vector (Dest : Data_Ptr;
                            Dest_Offset : Bit_Offset;
                            Dest_Width : Width_Type;
                            Dest_Type : Node;
                            Update : Update_Acc;
                            Val : Data_Ptr;
                            Voffset : Bit_Offset)
   is
      W : constant Width_Type := Get_Type_Width (Dest_Type);
      Change : Boolean;
   begin
      if Dest_Width = 0 then
         return;
      end if;
      pragma Assert (W >= Dest_Width);

      case Get_Kind (Dest_Type) is
         when N_Logic_Type =>
            pragma Assert (W = 1);
            pragma Assert (Voffset = 0);
            Compute_Log_Insert (To_Logvec_Ptr (Dest), Dest_Offset,
                                To_Logic_Ptr (Val).all, Change);
         when N_Bit_Type =>
            pragma Assert (W = 1);
            pragma Assert (Voffset = 0);
            Compute_Bit_Insert (To_Bitvec_Ptr (Dest), Dest_Offset,
                                To_Bit_Ptr (Val).all, Change);
         when N_Log_Packed_Array_Cst =>
            Compute_Part_Insert (To_Logvec_Ptr (Dest),
                                 Dest_Offset,
                                 To_Logvec_Ptr (Val),
                                 Voffset, Dest_Width, Change);
         when N_Bit_Packed_Array_Cst =>
            pragma Assert (Dest_Offset = 0);
            pragma Assert (Voffset = 0);
            Change := not Is_Eq (To_Bitvec_Ptr (Dest), To_Bitvec_Ptr (Val), W);
            Assign (To_Bitvec_Ptr (Dest), To_Bitvec_Ptr (Val), W);
         when N_Enum_Type =>
            pragma Assert (Update = null);
            Assign_Vector (Dest, Dest_Offset, Dest_Width,
                           Get_Enum_Base_Type (Dest_Type),
                           Update, Val, Voffset);
            return;
         when N_Packed_Struct_Type =>
            --  FIXME: mask 2 state fields
            pragma Assert (Update = null);
            Assign_Vector (Dest, Dest_Offset, Dest_Width,
                           Get_Packed_Base_Type (Dest_Type),
                           Update, Val, Voffset);
         when others =>
            raise Internal_Error;
      end case;

      if Change then
         Activate_Sensitized_Processes (Update);
      end if;
   end Assign_Vector;

   procedure Assign_Nonvec (Dest : Data_Ptr;
                            Dest_Type : Node;
                            Update : Update_Acc;
                            Val : Data_Ptr;
                            Val_Type : Node;
                            Loc : Node)
   is
      Change : Boolean;
   begin
      case Get_Kind (Dest_Type) is
         when N_Logic_Type =>
            declare
               Res : constant Logic_Type := To_Logic_Ptr (Val).all;
            begin
               Change := To_Logic_Ptr (Dest).all /= Res;
               To_Logic_Ptr (Dest).all := Res;
            end;
         when N_Bit_Type =>
            declare
               Res : constant Bit_Type := To_Bit_Ptr (Val).all;
            begin
               Change := To_Bit_Ptr (Dest).all /= Res;
               To_Bit_Ptr (Dest).all := Res;
            end;
         when N_Real_Type =>
            declare
               Res : constant Fp64 := To_Fp64_Ptr (Val).all;
            begin
               Change := To_Fp64_Ptr (Dest).all /= Res;
               To_Fp64_Ptr (Dest).all := Res;
            end;
         when N_Shortreal_Type =>
            declare
               Res : constant Fp32 := To_Fp32_Ptr (Val).all;
            begin
               Change := To_Fp32_Ptr (Dest).all /= Res;
               To_Fp32_Ptr (Dest).all := Res;
            end;
         when N_Array_Cst =>
            declare
               Len : constant Int32 := Compute_Length (Dest_Type);
               El_Type : constant Node := Get_Type_Element_Type (Dest_Type);
               Stride : constant Tsize_Type := Get_Stride_Size (Dest_Type);
               Sub_Dest : Data_Ptr;
               Sub_Val : Data_Ptr;
               Sub_Width : Width_Type;
            begin
               Change := False;
               Sub_Dest := Dest;

               --  Extract value address.
               case Get_Kind (Val_Type) is
                  when N_Array_Cst =>
                     Sub_Val := Val;
                  when N_Dynamic_Array_Cst =>
                     declare
                        Varr : constant Sv_Dyn_Array_Ptr :=
                          To_Sv_Dyn_Array_Ptr_Ptr (Val).all;
                     begin
                        if Varr = null or else Varr.Size /= Len then
                           Error_Msg_Exec
                             (+Loc, "size mismatch in assignment");
                        end if;
                        Sub_Val := Varr.Base(1)'Address;
                     end;
                  when others =>
                     Error_Kind ("assign_nonvec(array_cst)", Val_Type);
               end case;

               --  Assign
               case Nkinds_Types (Get_Kind (El_Type)) is
                  when N_Log_Packed_Array_Cst
                    | N_Bit_Packed_Array_Cst
                    | N_Packed_Struct_Type
                    | N_Packed_Union_Type =>
                     Sub_Width := Get_Type_Width (El_Type);
                     pragma Assert (Update = null); -- TODO.
                     for I in 0 .. Len - 1 loop
                        Assign_Vector
                          (Sub_Dest, 0, Sub_Width, El_Type, null, Sub_Val, 0);
                        Sub_Dest := Sub_Dest + Storage_Index (Stride);
                        Sub_Val := Sub_Val + Storage_Index (Stride);
                     end loop;
                  when N_Array_Cst
                    | N_Bit_Type
                    | N_Logic_Type =>
                     pragma Assert (Update = null); -- TODO.
                     pragma Assert
                       (Get_Type_Element_Type (Val_Type) = El_Type);
                     for I in 0 .. Len - 1 loop
                        Assign_Nonvec
                          (Sub_Dest, El_Type, null, Sub_Val, El_Type, Loc);
                        Sub_Dest := Sub_Dest + Storage_Index (Stride);
                        Sub_Val := Sub_Val + Storage_Index (Stride);
                     end loop;
                  when others =>
                     raise Internal_Error;
               end case;
            end;

         when N_Struct_Type =>
            declare
               Member : Node;
               Member_Type : Node;
               Off : Storage_Index;
            begin
               Change := False;
               pragma Assert (Update = null);
               Member := Get_Members (Dest_Type);
               while Member /= Null_Node loop
                  Member_Type := Get_Type_Data_Type (Member);
                  Off := Get_Unpacked_Member_Offset (Member);

                  case Nkinds_Types (Get_Kind (Member_Type)) is
                     when N_Log_Packed_Array_Cst
                       | N_Bit_Packed_Array_Cst
                       | N_Packed_Struct_Type
                       | N_Packed_Union_Type =>
                        Assign_Vector
                          (Dest + Off, 0, Get_Type_Width (Member_Type),
                           Member_Type, null, Val + Off, 0);
                     when N_Array_Cst
                       | N_Bit_Type
                       | N_Logic_Type
                       | N_Real_Type
                       | N_Shortreal_Type
                       | N_String_Type =>
                        Assign_Nonvec (Dest + Off, Member_Type, null,
                                       Val + Off, Member_Type, Loc);
                     when others =>
                        Error_Kind ("assign_nonvec(struct)", Member_Type);
                  end case;

                  Member := Get_Chain (Member);
               end loop;
            end;
         when N_String_Type =>
            pragma Assert (Update = null);
            Change := False;
            Unref (To_Sv_String_Ptr (Dest).all);
            To_Sv_String_Ptr (Dest).all := To_Sv_String_Ptr (Val).all;
         when Nkinds_Class =>
            pragma Assert (Update = null);
            Change := False;
            To_Sv_Class_Ptr (Dest).all := To_Sv_Class_Ptr (Val).all;
         when N_Dynamic_Array_Cst =>
            pragma Assert (Update = null);
            Change := False;
            declare
               Res : Sv_Dyn_Array_Ptr;
               Ssize : Storage_Index;
               Len : Int32;
            begin
               case Get_Kind (Val_Type) is
                  when N_Dynamic_Array_Cst =>
                     Res := To_Sv_Dyn_Array_Ptr_Ptr (Val).all;
                  when N_Array_Cst =>
                     Len := Compute_Length (Val_Type);
                     Res := Create_Dynamic_Array (Dest_Type, Len);
                     if Len > 0 then
                        Ssize := Storage_Index (Len)
                          * Storage_Index (Get_Stride_Size (Dest_Type));
                        Res.Base (1 .. Ssize) :=
                          To_Storage (Val).all (0 .. Ssize - 1);
                     end if;
                  when others =>
                     Error_Kind ("assign_nonvec(dynamic_array)", Val_Type);
               end case;
               --  FIXME: free old.
               To_Sv_Dyn_Array_Ptr_Ptr (Dest).all := Res;
            end;
         when N_Queue_Cst =>
            pragma Assert (Update = null);
            Change := False;
            case Get_Kind (Val_Type) is
               when N_Queue_Cst =>
                  Queue_Assign (To_Sv_Queue_Ptr (Dest).all,
                                To_Sv_Queue_Ptr (Val).all);
               when others =>
                  Error_Kind ("assign_nonvec(queue)", Val_Type);
            end case;
         when others =>
            Error_Kind ("assign_nonvec", Dest_Type);
      end case;

      if Change then
         Activate_Sensitized_Processes (Update);
      end if;
   end Assign_Nonvec;

   procedure Blocking_Assign_Lvalue
     (Frame : Frame_Ptr; Tgt : Node; Val : Data_Ptr; Etype : Node) is
   begin
      case Get_Kind (Tgt) is
         when N_Name
           | N_This_Name
           | N_Indexed_Name
           | N_Member_Name
           | N_Property_Name
           | N_Scoped_Name
           | N_Hierarchical
           | N_Interface_Item
           | N_Bit_Select
           | N_Part_Select_Cst
           | N_Var
           | Nkinds_Nets =>
            if Is_Vector_Name (Tgt, Get_Expr_Type (Tgt)) then
               declare
                  Dest : Data_Ptr;
                  Offset : Bit_Offset;
                  Doffset : Bit_Offset;
                  Width : Width_Type;
                  Update : Update_Acc;
               begin
                  Execute_Name_Vector
                    (Frame, Tgt, False, Dest, Offset, Doffset, Width, Update);
                  Assign_Vector
                    (Dest, Offset, Width, Etype, Update, Val, Doffset);
               end;
            else
               declare
                  Dest : Data_Ptr;
                  Update : Update_Acc;
               begin
                  Execute_Name_Nonvec (Frame, Tgt, False, Dest, Update);
                  Assign_Nonvec
                    (Dest, Get_Expr_Type (Tgt), Update, Val, Etype, Tgt);
               end;
            end if;
         when N_String_Index =>
            declare
               Pfx : constant Node := Get_Name (Tgt);
               Dest : Data_Ptr;
               Str : Sv_String;
               Update : Update_Acc;
               Index : Int32;
               Undef : Boolean;
               C : Uns32;
            begin
               Execute_Name_Nonvec (Frame, Pfx, False, Dest, Update);
               Str := To_Sv_String_Ptr (Dest).all;
               Execute_Expression_Int32
                 (Frame, Get_Expression (Tgt), Index, Undef);
               C := To_Bitvec_Ptr (Val)(0) and 16#ff#;
               if not Undef
                 and Index >= 0
                 and Index < Int32 (Get_Length (Str))
                 and C /= 0
               then
                  --  Ensure this is the only string reference.
                  Make_Unique (Str);
                  To_Sv_String_Ptr (Dest).all := Str;
                  Set_String_El (Str, Positive (Index + 1), Character'Val (C));
               end if;
            end;
         when N_Associative_Index =>
            declare
               Pfx : constant Node := Get_Name (Tgt);
               Idx_Expr : constant Node := Get_Expression (Tgt);
               Idx_Type : constant Node := Get_Expr_Type (Idx_Expr);
               Idx_Ssize : constant Storage_Index :=
                 Get_Storage_Size (Idx_Type);
               Idx : Storage_Type (0 .. Idx_Ssize - 1);
               Dest : Data_Ptr;
               Map : Sv_Map;
               Update : Update_Acc;
            begin
               Execute_Name_Nonvec (Frame, Pfx, False, Dest, Update);
               Map := To_Sv_Map_Ptr (Dest).all;
               Execute_Expression (Frame, Idx'Address, Idx_Expr);
               Set_Map (Map, Idx'Address, Val);
            end;
         when N_Concatenation =>
            declare
               El : Node;
               Expr : Node;
               Offset : Width_Type;
               El_Type : Node;
            begin
               Offset := Get_Type_Width (Etype);
               El := Get_Expressions (Tgt);
               while El /= Null_Node loop
                  Expr := Get_Expression (El);
                  El_Type := Get_Expr_Type (Expr);
                  case Get_Kind (El_Type) is
                     when N_Log_Packed_Array_Cst =>
                        declare
                           El_Width : constant Width_Type :=
                             Get_Type_Width (El_Type);
                           Ssize : constant Storage_Index :=
                             Get_Storage_Size (El_Type);
                           El_Res : Storage_Type (0 .. Ssize - 1);
                        begin
                           pragma Assert (Offset >= El_Width);
                           Offset := Offset - El_Width;
                           Compute_Part_Select
                             (To_Logvec_Ptr (El_Res'Address),
                              To_Logvec_Ptr (Val),
                              Bit_Offset (Offset), El_Width);
                           Blocking_Assign_Lvalue
                             (Frame, Expr, El_Res'Address, El_Type);
                        end;
                     when N_Logic_Type =>
                        declare
                           El_Res : Logic_Type;
                        begin
                           pragma Assert (Offset >= 1);
                           Offset := Offset - 1;
                           El_Res := Compute_Bit_Select
                             (To_Logvec_Ptr (Val), Bit_Offset (Offset));
                           Blocking_Assign_Lvalue
                             (Frame, Expr, El_Res'Address, El_Type);
                        end;
                     when others =>
                        Error_Kind ("blocking_assign_lvalue(concat)", El_Type);
                  end case;
                  El := Get_Chain (El);
               end loop;
               pragma Assert (Offset = 0);
            end;
         when others =>
            Error_Kind ("blocking_assign_lvalue", Tgt);
      end case;
   end Blocking_Assign_Lvalue;

   procedure Execute_Blocking_Assign
     (Frame : Frame_Ptr; Tgt : Node; Expr : Node)
   is
      Expr_Type : constant Node := Get_Expr_Type (Expr);
      Ssize : constant Storage_Index := Get_Storage_Size (Expr_Type);
      Res : Storage_Type (0 .. Ssize - 1);
   begin
      Execute_Expression (Frame, Res'Address, Expr);

      if Flag_Trace then
         Trace (" >>> ");
         Allocates.Disp_Value (Res'Address, Expr_Type);
         Trace_Newline;
      end if;

      Blocking_Assign_Lvalue (Frame, Tgt, Res'Address, Expr_Type);
   end Execute_Blocking_Assign;

   procedure Execute_Blocking_Assign (Frame : Frame_Ptr; Asgn : Node) is
   begin
      if Flag_Trace then
         Trace_Head;
         Disp_Verilog.Disp_Blocking_Assignment (Asgn);
      end if;

      Execute_Blocking_Assign
        (Frame, Get_Lvalue (Asgn), Get_Expression (Asgn));
   end Execute_Blocking_Assign;

   --  Like Execute_Blocking_Assign, but VAR is a declaration.  Simpler.
   --  Used for initialization and implicit assign declarations (wire x = e).
   procedure Execute_Implicit_Assign
     (Frame : Frame_Ptr; Var : Node; Expr : Node)
   is
      Vtype : constant Node := Get_Type_Data_Type (Var);
      Ssize : constant Storage_Index := Get_Storage_Size (Vtype);
      Res : Storage_Type (0 .. Ssize - 1);
      Value : Data_Ptr;
      Update : Update_Acc;
   begin
      if Flag_Trace then
         Trace_Head;
         Disp_Verilog.Disp_Expression (Var);
      end if;

      Execute_Expression (Frame, Res'Address, Expr);

      if Flag_Trace then
         Trace (" >>> ");
         Allocates.Disp_Value (Res'Address, Vtype);
         Trace_Newline;
      end if;

      Value := Get_Var_Data (Frame, Var);
      if not Get_Is_Automatic (Var) then
         Update := Get_Var_Update (Var);
      else
         Update := null;
      end if;

      case Get_Kind (Vtype) is
         when N_Log_Packed_Array_Cst
           | N_Bit_Packed_Array_Cst
           | N_Enum_Type =>
            Assign_Vector (Value, 0, Get_Type_Width (Vtype), Vtype,
                           Update, Res'Address, 0);
         when N_Array_Cst
           | Nkinds_Class
           | N_String_Type
           | N_Logic_Type
           | N_Bit_Type
           | N_Struct_Type
           | N_Real_Type =>
            Assign_Nonvec (Value, Vtype, Update, Res'Address, Vtype, Expr);
         when others =>
            --  Execute_Blocking_Assign (Frame, Var, Expr);
            Error_Kind ("execute_implicit_assign", Vtype);
      end case;
   end Execute_Implicit_Assign;

   procedure Execute_Continuous_Assign (Asgn : Node) is
   begin
      if Flag_Trace then
         Trace_Head;
         Disp_Verilog.Disp_Continuous_Assignment (Asgn);
      end if;

      Execute_Blocking_Assign
        (null, Get_Lvalue (Asgn), Get_Expression (Asgn));
   end Execute_Continuous_Assign;

   procedure Execute_Non_Blocking_Assign (Asgn : Node)
   is
      Expr : constant Node := Get_Expression (Asgn);
      Expr_Type : constant Node := Get_Expr_Type (Expr);
      Ssize : constant Storage_Index := Get_Storage_Size (Expr_Type);
      Tgt : constant Node := Get_Lvalue (Asgn);
      Ev : Event_Acc;
   begin
      if Flag_Trace then
         Trace_Head;
         Disp_Verilog.Disp_Non_Blocking_Assignment (Asgn);
      end if;

      if Is_Vector_Name (Tgt, Expr_Type) then
         Ev := new Event_Type (Kind => Nba_Vector_Event,
                               Size => Ssize);
         Execute_Name_Vector
           (null, Tgt, False,
            Ev.Target_Data, Ev.Target_Offset, Ev.Target_Doffset,
            Ev.Target_Width, Ev.Target_Update);
      else
         Ev := new Event_Type (Kind => Nba_Nonvec_Event,
                               Size => Ssize);

         Execute_Name_Nonvec
           (null, Tgt, False, Ev.Target_Data, Ev.Target_Update);
         Ev.Target_Offset := 0;
         Ev.Target_Width := 1;
      end if;

      Ev.Target_Type := Expr_Type;
      Execute_Expression (null, Ev.Value'Address, Expr);

      if Flag_Trace then
         Trace (" >>> ");
         Allocates.Disp_Value (Ev.Value'Address, Expr_Type);
         Trace_Newline;
      end if;

      if Ev.Target_Data = No_Data_Ptr then
         Free (Ev);
         return;
      end if;

      Append (Current_Entry.Nba, Ev);
   end Execute_Non_Blocking_Assign;

   procedure Execute_System_Task (Frame : Frame_Ptr; Call : Node)
   is
      Call_Type : constant Node := Get_Expr_Type (Call);
      Id : constant Sys_Tf_Id := Get_Sys_Tf_Id (Call);
   begin
      if Call_Type /= Null_Node then
         --  This is a function.
         declare
            Size : constant Storage_Index := Get_Storage_Size (Call_Type);
            Res : Storage_Type (0 .. Size - 1);
         begin
            Init (Res'Address, Call_Type);
            Verilog.Vpi.Call_Sysfunc_Calltf (Frame, Id, Call, Res'Address);
         end;
      else
         --  This is a task.
         Verilog.Vpi.Call_Systask_Calltf (Frame, Id, Call);
      end if;
   end Execute_System_Task;

   --  Get the next statement to execute after INSN.
   function Next_Insn (Frame : Frame_Ptr; Insn : Node) return Node;

   function Execute_If_Statement (Frame : Frame_Ptr; Insn : Node) return Node
   is
      Cond : Tri_State_Type;
      Insn1 : Node;
   begin
      if Flag_Trace then
         Trace_Head;
         Verilog.Disp_Verilog.Disp_If_Header (Insn);
      end if;

      Cond := Execute_Condition (Frame, Get_Condition (Insn));

      if Flag_Trace then
         Trace (" >>>");
         Trace_Cond (Cond);
         Trace_Newline;
      end if;

      --  IEEE 1364-2005 9.4 Conditional statement
      --  If the expression evaluates to true (that is, has a nonzero known
      --  value), the first statement shall be executed.  If it evaluates to
      --  false (has a zero value or the value is x or z), the first statement
      --  shall not execute.  If there is an else statement and expression is
      --  false, the else statement shall be executed.
      if Cond = True then
         return Get_True_Stmt (Insn);
      else
         if Cond = Unknown then
            --  Emit a warning ?
            pragma Assert (True);
            null;
         end if;
         Insn1 := Get_False_Stmt (Insn);
         if Insn1 = Null_Node then
            return Next_Insn (Frame, Insn);
         else
            return Insn1;
         end if;
      end if;
   end Execute_If_Statement;

   function Execute_For_Statement (Frame : Frame_Ptr; Insn : Node) return Node
   is
      Init : constant Node := Get_For_Initialization (Insn);
      Cond : Tri_State_Type;
   begin
      if Get_Kind (Init) = N_Var then
         Execute_Declarations (Frame, Init);
      else
         Execute_Blocking_Assign (Frame, Init);
      end if;

      if Flag_Trace then
         Trace_Head;
         Disp_Verilog.Disp_Expression (Get_Condition (Insn));
      end if;

      Cond := Execute_Condition (Frame, Get_Condition (Insn));

      if Flag_Trace then
         Trace (" >>>");
         Trace_Cond (Cond);
         Trace_Newline;
      end if;

      if Cond = True then
         return Get_Statement (Insn);
      else
         return Next_Insn (Frame, Insn);
      end if;
   end Execute_For_Statement;

   function Execute_For_Statement_Resume
     (Frame : Frame_Ptr; Insn : Node) return Node
   is
      Cond : Tri_State_Type;
      Stmt : Node;
   begin
      Stmt := Get_Step_Assign (Insn);
      while Stmt /= Null_Node loop
         Execute_Simple_Statement (Frame, Stmt);
         Stmt := Get_Chain (Stmt);
      end loop;

      if Flag_Trace then
         Trace_Head;
         Disp_Verilog.Disp_Expression (Get_Condition (Insn));
      end if;

      Cond := Execute_Condition (Frame, Get_Condition (Insn));

      if Flag_Trace then
         Trace (" >>>");
         Trace_Cond (Cond);
         Trace_Newline;
      end if;

      if Cond = True then
         return Get_Statement (Insn);
      else
         return Next_Insn (Frame, Insn);
      end if;
   end Execute_For_Statement_Resume;

   function Execute_While_Statement
     (Frame : Frame_Ptr; Insn : Node) return Node
   is
      Cond : Tri_State_Type;
   begin
      Cond := Execute_Condition (Frame, Get_Condition (Insn));

      if Cond = True then
         return Get_Statement (Insn);
      else
         return Next_Insn (Frame, Insn);
      end if;
   end Execute_While_Statement;

   function Execute_Case_Statement (Frame : Frame_Ptr; Insn : Node) return Node
   is
      Stmt_Kind : constant Nkind := Get_Kind (Insn);
      Expr : constant Node := Get_Expression (Insn);
      Expr_Type : constant Node := Get_Expr_Type (Expr);
      Ssize : constant Storage_Index := Get_Storage_Size (Expr_Type);
      Res : Storage_Type (0 .. Ssize - 1);
      Item : Node;
      Stmt : Node;
      Default : Node;
   begin
      --  1800-2017 12.5 Case statement
      --  The case expression shall be evaluated exactly once and before any
      --  of the case_item_expressions.
      Execute_Expression (Frame, Res'Address, Expr);

      if Flag_Trace then
         Trace (" >>> ");
         Allocates.Disp_Value (Res'Address, Expr_Type);
         Trace_Newline;
      end if;

      Item := Get_Case_Items (Insn);
      Default := Null_Node;
      while Item /= Null_Node loop
         case Nkinds_Case_Item (Get_Kind (Item)) is
            when N_Default_Case_Item =>
               --  1800-2017 12.5 Case statement
               --  If there is a default case_item, it is ignored during this
               --  linear search.
               Default := Item;
            when N_Case_Item =>
               --  1800-2017 12.5 Case statement
               --  The case_item_expressions shall be evaluated and then
               --  compared in the exact order in which they appear.
               declare
                  Case_Expr : constant Node := Get_Expression (Item);
                  Case_Type : constant Node := Get_Expr_Type (Case_Expr);
                  Case_Size : constant Storage_Index :=
                    Get_Storage_Size (Case_Type);
                  Case_Val : Storage_Type (0 .. Case_Size - 1);
                  R : Boolean;
               begin
                  Execute_Expression (Frame, Case_Val'Address, Case_Expr);
                  pragma Assert (Case_Size = Ssize);
                  case Nkinds_Case (Stmt_Kind) is
                     when N_Case =>
                        R := Is_Eq (Res'Address, Case_Val'Address, Expr_Type);
                     when N_Casex =>
                        R := Is_Eqx (Res'Address, Case_Val'Address, Expr_Type);
                     when N_Casez =>
                        R := Is_Eqz (Res'Address, Case_Val'Address, Expr_Type);
                  end case;

                  if R then
                     --  Find the item with the statement.
                     while Get_Same_Case_Flag (Item) loop
                        Item := Get_Chain (Item);
                     end loop;
                     exit;
                  end if;
               end;
         end case;
         Item := Get_Chain (Item);
      end loop;

      if Item = Null_Node then
         --  No choice, continue.
         if Default = Null_Node then
            return Next_Insn (Frame, Insn);
         else
            Item := Default;
         end if;
      end if;

      Stmt := Get_Statement (Item);
      if Stmt = Null_Node then
         return Next_Insn (Frame, Insn);
      else
         return Stmt;
      end if;
   end Execute_Case_Statement;

   procedure Execute_Failure (Insn : Node; S : String) is
   begin
      --  TODO: hierarchy...

      Put (Files_Map.Image (Get_Location (Insn)));
      Put (": ");
      Put (S);
      Put (" failure at time #");
      Put_Uns32 (Time);
      New_Line;
   end Execute_Failure;

   function Execute_Immediate_Assert (Frame : Frame_Ptr; Insn : Node)
                                      return Node
   is
      Cond : Tri_State_Type;
      Res : Node;
   begin
      if Flag_Trace then
         Trace_Head;
         --  Verilog.Disp_Verilog.Disp_If_Header (Insn);
      end if;

      Cond := Execute_Condition (Frame, Get_Condition (Insn));

      if Flag_Trace then
         Trace (" >>>");
         Trace_Cond (Cond);
         Trace_Newline;
      end if;

      --  IEEE 1800-2017 16.3 Immediate assertions
      if Cond = True then
         Res := Get_Pass_Stmt (Insn);
         if Res = Null_Node then
            return Next_Insn (Frame, Insn);
         else
            return Res;
         end if;
      else
         if Cond = Unknown then
            --  Emit a warning ?
            pragma Assert (True);
            null;
         end if;
         --  IEEE 1800-2017 16.3 Immediate assertions
         --  If an assert statement fails and no else clause is specified, the
         --  tool shall, by default, call $error, unless $assertcontrol with
         --  control_type 9 (FailOff) is used to suppress the failure.
         Res := Get_Else_Stmt (Insn);
         if Res = Null_Node then
            Execute_Failure (Insn, "assert");
            return Next_Insn (Frame, Insn);
         else
            return Res;
         end if;
      end if;
   end Execute_Immediate_Assert;

   procedure Execute_Delay_Control (Proc : Process_Acc; Insn : Node)
   is
      Is_Undef : Boolean;
      V : Int32;
      Val : Uns32;
      Idx : Uns32;
   begin
      Execute_Expression_Int32 (null, Get_Expression (Insn), V, Is_Undef);

      --  IEEE 1364-2005 9.7.1 Delay control
      --  If the delay expression evaluates to an unknown or high-impedance
      --  value, it shall be interpreted as a zero delay.
      if Is_Undef then
         Val := 0;
      else
         Val := Uns32 (V);
      end if;

      Proc.Wakeup := Time + Val;

      if Val = 0 then
         --  IEEE 1364-2005 11.3 The stratified event queue
         --  An explicit zero delay (#0) requires that the process be
         --  suspended and added as an inactive event for the current
         --  time so that the process is resumed in the next simulation
         --  cycle in the current time.

         Append (Current_Entry.Inactive, Proc);
      else
         if Val >= Time_Wheel_Size then
            --  Value is too far in the future for time_wheel.
            if (Val / Time_Wheel_Size) > Time_Wheel_Size then
               --  And also too far for time_wheel2.
               Append (Time_Wheel3.Inactive, Proc);
            else
               Idx := (Time + Val) / Time_Wheel_Size;
               Append (Time_Wheel2 (Idx mod Time_Wheel_Size).Inactive, Proc);
            end if;
         else
            Idx := Time + Val;
            Append (Time_Wheel (Idx mod Time_Wheel_Size).Inactive, Proc);
         end if;
      end if;

      Proc.Pause := 1;
   end Execute_Delay_Control;

   procedure Execute_Return_Statement (Frame : Frame_Ptr; Insn : Node)
   is
      Expr : constant Node := Get_Expression (Insn);
      Retvar : Node;
   begin
      if Expr = Null_Node then
         return;
      end if;
      --  Assign the return variable.
      Retvar := Get_Return_Variable_Ref (Insn);
      Execute_Expression (Frame, Get_Var_Data (Frame, Retvar), Expr);
   end Execute_Return_Statement;

   function Next_Insn (Frame : Frame_Ptr; Insn : Node) return Node
   is
      Res : Node;
      Chain : Node;
   begin
      Res := Insn;

      loop
         Chain := Get_Chain (Res);
         if Chain /= Null_Node then
            return Chain;
         end if;
         Res := Get_Parent (Res);
         case Get_Kind (Res) is
            when Nkinds_Process
              | Nkinds_Any_Tf =>
               --  End of initial/always.
               return Null_Node;
            when N_For =>
               return Execute_For_Statement_Resume (Frame, Res);
            when N_While =>
               return Execute_While_Statement (Frame, Res);
            when N_Forever =>
               return Get_Statement (Res);
            when N_Repeat =>
               declare
                  Dest : constant Data_Ptr := Get_Var_Data (Frame, Res);
                  Bv : constant Bitvec_Ptr := To_Bitvec_Ptr (Dest);
               begin
                  Bv (0) := Bv (0) - 1;
                  if Bv (0) = 0 then
                     return Next_Insn (Frame, Res);
                  else
                     return Get_Statement (Res);
                  end if;
               end;
            when N_Seq_Block =>
               Finalize_Declarations
                 (Frame, Get_Block_Item_Declaration_Chain (Res));
            when N_If
              | N_Delay_Control
              | N_Event_Control
              | Nkinds_Case =>
               null;
            when others =>
               Error_Kind ("next_insn", Res);
         end case;
      end loop;
   end Next_Insn;

   --  Some statements (like delay_control) have a sub-statement which can
   --  be null.  This function returns the next statement to be executed after
   --  INSN for that case.
   function Get_Statement_Or_Next (Frame : Frame_Ptr; Insn : Node) return Node
   is
      Res : constant Node := Get_Statement (Insn);
   begin
      if Res /= Null_Node then
         return Res;
      else
         return Next_Insn (Frame, Insn);
      end if;
   end Get_Statement_Or_Next;

   procedure Execute_Simple_Statement (Frame : Frame_Ptr; Insn : Node) is
   begin
      case Get_Kind (Insn) is
         when N_Blocking_Assign =>
            Execute_Blocking_Assign (Frame, Insn);
         when N_Noblk_Assign =>
            Execute_Non_Blocking_Assign (Insn);
         when N_Post_Increment =>
            declare
               Etype : constant Node := Get_Expr_Type (Insn);
               Ssize : constant Storage_Index := Get_Storage_Size (Etype);
               Res : Storage_Type (0 .. Ssize - 1);
            begin
               Execute_Expression (Frame, Res'Address, Insn);
            end;
         when N_Assign_Operator =>
            Execute_Assign_Operator (Frame, Insn);
         when others =>
            Error_Kind ("execute_simple_statement", Insn);
      end case;
   end Execute_Simple_Statement;

   procedure Execute_Statements
     (Link : in out Frame_Link_Type; Proc : Process_Acc)
   is
      Insn : Node;
      Frame : Frame_Ptr;
   begin
      Insn := Link.Pc;
      Frame := Link.Frame;

      Main_Loop: loop
         while Insn = Null_Node loop
            --  End of process or static routine.
            case Get_Kind (Link.Origin) is
               when N_Initial
                 | N_Always
                 | N_Always_Comb
                 | N_Debug =>
                  exit Main_Loop;
               when N_Function
                 | N_Extern_Function =>
                  --  Need to get the result stored in the current frame.
                  exit Main_Loop;
               when N_Task
                 | N_Extern_Task =>
                  --  TODO: copy output and inout arguments.
                  Finalize_Declarations
                    (Frame, Get_Tf_Item_Declaration_Chain (Link.Origin));
                  declare
                     Old_Frame : Frame_Ptr;
                  begin
                     Old_Frame := Frame;
                     Link := To_Frame_Link_Ptr (Old_Frame).all;
                     Frame := Link.Frame;
                     Insn := Link.Pc;
                     Deallocate_Frame (Old_Frame);
                  end;
               when others =>
                  Error_Kind ("execute_statements(end)", Link.Origin);
            end case;
         end loop;

         case Get_Kind (Insn) is
            when N_Seq_Block =>
               if Flag_Trace then
                  Trace_Head;
                  Trace ("begin");
                  Trace_Newline;
               end if;
               Execute_Declarations
                 (Frame, Get_Block_Item_Declaration_Chain (Insn));
               Insn := Get_Statements_Chain (Insn);
            when N_Blocking_Assign =>
               Execute_Blocking_Assign (Frame, Insn);
               Insn := Next_Insn (Frame, Insn);
            when N_Noblk_Assign =>
               Execute_Non_Blocking_Assign (Insn);
               Insn := Next_Insn (Frame, Insn);
            when N_Post_Increment
              | N_Assign_Operator =>
               if Flag_Trace then
                  Trace_Head;
                  Trace ("assign");
                  Trace_Newline;
               end if;
               Execute_Simple_Statement (Frame, Insn);
               Insn := Next_Insn (Frame, Insn);
            when N_Delay_Control =>
               if Proc = null then
                  raise Program_Error;
               end if;
               if Flag_Trace then
                  Trace_Head;
                  Verilog.Disp_Verilog.Disp_Control (Insn);
               end if;

               if Proc.Pause = 0 then
                  --  First execution.
                  if Flag_Trace then
                     Trace (" >>> suspend");
                     Trace_Newline;
                  end if;
                  Execute_Delay_Control (Proc, Insn);
                  exit Main_Loop;
               else
                  --  Resume.
                  if Flag_Trace then
                     Trace (" >>> resume");
                     Trace_Newline;
                  end if;
                  Proc.Pause := 0;
                  Insn := Get_Statement_Or_Next (Frame, Insn);
               end if;
            when N_Event_Control =>
               pragma Assert (Proc /= null);
               if Flag_Trace then
                  Trace_Head;
                  Verilog.Disp_Verilog.Disp_Control (Insn);
               end if;
               if Proc.Pause = 0 then
                  if Flag_Trace then
                     Trace (" >>> suspend");
                     Trace_Newline;
                  end if;
                  Proc.Pause := 1;
                  exit Main_Loop;
               else
                  --  Resume.
                  if Flag_Trace then
                     Trace (" >>> resume");
                     Trace_Newline;
                  end if;
                  Proc.Pause := 0;
                  Insn := Get_Statement_Or_Next (Frame, Insn);
               end if;
            when N_Repeat_Control =>
               if Flag_Trace then
                  Trace_Head;
                  Verilog.Disp_Verilog.Disp_Control (Insn);
               end if;
               pragma Assert (Proc /= null);
               if Proc.Pause = 0 then
                  declare
                     Is_Undef : Boolean;
                     V : Int32;
                  begin
                     Execute_Expression_Int32
                       (null, Get_Expression (Insn), V, Is_Undef);
                     --  1800-2017 9.4.5 Intra-assignment timing controls
                     --  If the repeat count literal, or signed variable
                     --  holding the repeat count, is less than or equal to 0
                     --  at the time of evaluation, the assignment occurs as if
                     --  there is no repeat construct.
                     --  GHDL: what about undef ?
                     if Is_Undef or V <= 0 then
                        V := 0;
                     end if;
                     Proc.Count := V;
                     Proc.Pause := 1;
                  end;
               else
                  Proc.Count := Proc.Count - 1;
               end if;
               if Proc.Count = 0 then
                  Proc.Pause := 0;
                  if Flag_Trace then
                     Trace (" >>> resume");
                     Trace_Newline;
                  end if;
                  Proc.Pause := 0;
                  Insn := Get_Statement_Or_Next (Frame, Insn);
               else
                  if Flag_Trace then
                     Trace (" >>> suspend");
                     Trace_Newline;
                  end if;
                  exit Main_Loop;
               end if;
            when N_Subroutine_Call_Stmt =>
               if Flag_Trace then
                  Trace_Head;
                  Disp_Verilog.Disp_Expression (Get_Call (Insn));
                  Trace_Newline;
               end if;
               declare
                  Call : constant Node := Get_Call (Insn);
                  Rtn : Node;
                  Subprg : Node;
                  New_Link : Frame_Link_Type;
                  Handle : Sv_Class_Handle;
               begin
                  if Get_Kind (Call) = N_System_Call then
                     Execute_System_Task (Frame, Call);
                     Insn := Next_Insn (Frame, Insn);
                     exit Main_Loop when Verilog.Vpi.Vpip_Control /= 0;
                  else
                     pragma Assert (Get_Kind (Call) = N_Call);
                     Rtn := Get_Subroutine (Call);
                     Subprg := Get_Declaration (Rtn);
                     case Get_Kind (Subprg) is
                        when N_Function
                          | N_Extern_Function =>
                           declare
                              Ftype : constant Node :=
                                Get_Type_Data_Type (Subprg);
                              Ssize : constant Storage_Index :=
                                Get_Storage_Size (Ftype);
                              Res : Storage_Type (1 .. Ssize);
                           begin
                              Execute_Expression (Frame, Res'Address, Call);
                              --  FIXME: free result.
                              Insn := Next_Insn (Frame, Insn);
                           end;
                        when N_Task
                          | N_Extern_Task =>
                           if Get_Kind (Rtn) = N_Method_Name then
                              Execute_Expression
                                (Frame, Handle'Address, Get_Name (Rtn));
                           else
                              Handle := null;
                           end if;
                           Prepare_Call
                             (Frame, Call, Subprg, Handle, New_Link);
                           --  The return insn is the next insn.
                           Insn := Next_Insn (Frame, Insn);
                           --  Save frame.
                           To_Frame_Link_Ptr (New_Link.Frame).all :=
                             (Origin => Link.Origin,
                              Pc => Insn,
                              Frame => Frame);
                           --  Execute task.
                           Link := New_Link;
                           Frame := Link.Frame;
                           Insn := Link.Pc;
                        when others =>
                           raise Internal_Error;
                     end case;
                  end if;
               end;
            when N_Simple_Immediate_Assert =>
               Insn := Execute_Immediate_Assert (Frame, Insn);
            when N_If =>
               Insn := Execute_If_Statement (Frame, Insn);
            when N_While =>
               if Flag_Trace then
                  Trace_Head;
                  Disp_Verilog.Disp_While_Header (Insn);
                  Trace_Newline;
               end if;
               Insn := Execute_While_Statement (Frame, Insn);
            when N_For =>
               if Flag_Trace then
                  Trace_Head;
                  Disp_Verilog.Disp_For_Header (Insn);
                  Trace_Newline;
               end if;
               Insn := Execute_For_Statement (Frame, Insn);
            when N_Forever =>
               if Flag_Trace then
                  Trace_Head;
                  Trace ("forever");
                  Trace_Newline;
               end if;
               Insn := Get_Statement (Insn);
            when N_Repeat =>
               if Flag_Trace then
                  Trace_Head;
                  Trace ("repeat");
                  Trace_Newline;
               end if;
               declare
                  V : Int32;
                  Is_Undef : Boolean;
                  Dest : Data_Ptr;
               begin
                  Execute_Expression_Int32
                    (null, Get_Expression (Insn), V, Is_Undef);
                  --  1800-2017 12.7.2 The repeat loop
                  --  If the expression evaluates to unknown or high impedance,
                  --  it shall be treated as zero, and no statement shall be
                  --  executed.
                  if V <= 0 or Is_Undef then
                     Insn := Next_Insn (Frame, Insn);
                  else
                     Dest := Get_Var_Data (Frame, Insn);
                     To_Bitvec_Ptr (Dest)(0) := Uns32 (V);
                     Insn := Get_Statement_Or_Next (Frame, Insn);
                  end if;
               end;
            when N_Return_Stmt =>
               if Flag_Trace then
                  Trace_Head;
                  Trace ("return");
                  Trace_Newline;
               end if;
               Execute_Return_Statement (Frame, Insn);
               Insn := Null_Node;
            when Nkinds_Case =>
               if Flag_Trace then
                  Trace_Head;
                  Disp_Verilog.Disp_Case_Header (Insn);
               end if;
               Insn := Execute_Case_Statement (Frame, Insn);
            when others =>
               Error_Kind ("execute_statements", Insn);
         end case;
      end loop Main_Loop;

      Link.Pc := Insn;
      Link.Frame := Frame;
   end Execute_Statements;

   procedure Free_Unreferenced_Process (Proc : Process_Acc)
   is
      Proc1 : Process_Acc;
   begin
      --  Unreferenced and not in suspended state.
      if Proc.Nref = 0
        and then (Proc.Kind /= Process_User or else Proc.Pause = 0)
      then
         Proc1 := Proc;
         Free (Proc1);
      end if;
   end Free_Unreferenced_Process;

   procedure Execute_Process (Proc : Process_Acc) is
   begin
      if Flag_Trace_Exec or Flag_Trace then
         Put ("execute process at ");
         Put (Files_Map.Image (Get_Location (Proc.Stmt)));
         New_Line;
      end if;

      loop
         Execute_Statements (Proc.Link, Proc);

         if Proc.Link.Pc = Null_Node then
            case Get_Kind (Proc.Stmt) is
               when N_Initial =>
                  --  End of process, won't be executed again.
                  exit;
               when N_Debug
                 | N_Always_Comb =>
                  --  End of process.
                  exit;
               when N_Always =>
                  --  Start again.
                  Proc.Link.Pc := Get_Statement (Proc.Stmt);
               when others =>
                  Error_Kind ("execute_process(2)", Proc.Stmt);
            end case;
         else
            --  Suspended.
            exit;
         end if;
      end loop;
   end Execute_Process;

   procedure Execute_Assign (Proc : Process_Acc) is
   begin
      if Flag_Trace_Exec or Flag_Trace then
         Put ("execute assign at ");
         Put (Files_Map.Image (Get_Location (Proc.Stmt)));
         New_Line;
      end if;

      --  FIXME: delay
      Execute_Continuous_Assign (Proc.Stmt);
   end Execute_Assign;

   procedure Execute_Implicit_Assign (Proc : Process_Acc) is
   begin
      if Flag_Trace_Exec or Flag_Trace then
         Put ("execute net at ");
         Put (Files_Map.Image (Get_Location (Proc.Stmt)));
         New_Line;
      end if;
      if Flag_Trace then
         Trace_Head;
         Disp_Verilog.Disp_One_Net_Declaration (0, Proc.Stmt);
      end if;

      --  FIXME: delay
      Execute_Implicit_Assign (null, Proc.Stmt, Get_Expression (Proc.Stmt));
   end Execute_Implicit_Assign;

   type Logic_Array is array (Logic_Type, Logic_Type) of Logic_Type;
   type Logic_Gate_Config is record
      Init : Logic_Type;
      Upd : Logic_Array;
   end record;

   Logic_Gate_And : constant Logic_Gate_Config :=
     (Init => V_1,
      Upd => (V_0 => (others => V_0),
              V_1 => (V_0 => V_0, V_1 => V_1, V_X | V_Z => V_X),
              V_X | V_Z => (V_0 => V_0, others => V_X)));
   Logic_Gate_Or : constant Logic_Gate_Config :=
     (Init => V_0,
      Upd => (V_1 => (others => V_1),
              V_0 => (V_0 => V_0, V_1 => V_1, V_X | V_Z => V_X),
              V_X | V_Z => (V_1 => V_1, others => V_X)));

   Logic_Gate_Xor : constant Logic_Gate_Config :=
     (Init => V_0,
      Upd => (V_1 => (V_0 => V_1, V_1 => V_0, others => V_X),
              V_0 => (V_1 => V_1, V_0 => V_0, others => V_X),
              V_X | V_Z => (others => V_X)));

   procedure Execute_Logic_Gate (Gate : Node; Cfg : Logic_Gate_Config)
   is
      Output_Term : constant Node := Get_Gate_Terminals (Gate);
      Output : constant Node := Get_Expression (Output_Term);
      Input : Node;
      Val : Logic_Type;
      Res : Logic_Type;
   begin
      Input := Get_Chain (Output_Term);
      Res := Cfg.Init;
      loop
         Execute_Expression (null, Val'Address, Get_Expression (Input));
         Res := Cfg.Upd (Res, Val);
         Input := Get_Chain (Input);
         exit when Input = Null_Node;
      end loop;
      Blocking_Assign_Lvalue
        (Null_Frame, Output, Res'Address, Get_Expr_Type (Output));
   end Execute_Logic_Gate;

   procedure Execute_Gate (Proc : Process_Acc)
   is
      Gate : constant Node := Proc.Stmt;
   begin
      if Flag_Trace_Exec or Flag_Trace then
         Put ("execute gate ");
         Disp_Verilog.Disp_Gate_Kind (Gate);
         Put (" at ");
         Put (Files_Map.Image (Get_Location (Gate)));
         New_Line;
      end if;

      case Get_Kind (Gate) is
         when N_Gate_And =>
            Execute_Logic_Gate (Gate, Logic_Gate_And);
         when N_Gate_Xor =>
            Execute_Logic_Gate (Gate, Logic_Gate_Xor);
         when N_Gate_Or =>
            Execute_Logic_Gate (Gate, Logic_Gate_Or);
         when others =>
            Error_Kind ("execute_gate", Gate);
      end case;
   end Execute_Gate;

   procedure Execute_Conn_Input (Proc : Process_Acc)
   is
      Conn : constant Node := Proc.Stmt;

      Expr : constant Node := Get_Expression (Conn);
      Port : Node;
   begin
      if Flag_Trace_Exec or Flag_Trace then
         Put ("execute conn input at ");
         Put (Files_Map.Image (Get_Location (Proc.Stmt)));
         New_Line;
      end if;

      Port := Get_Port (Conn);
      if Get_Kind (Port) = N_Port then
         Port := Get_Expression (Port);
         pragma Assert (Get_Kind (Port) = N_Name);
         Port := Get_Declaration (Port);
      end if;
      pragma Assert (Get_Kind (Port) = N_Input);
      Port := Get_Redeclaration (Port);

      Execute_Implicit_Assign (null, Port, Expr);
   end Execute_Conn_Input;

   procedure Execute_Conn_Output (Proc : Process_Acc)
   is
      Conn : constant Node := Proc.Stmt;

      Expr : constant Node := Get_Expression (Conn);

      Port : Node;
   begin
      if Flag_Trace_Exec or Flag_Trace then
         Put ("execute conn output at ");
         Put (Files_Map.Image (Get_Location (Proc.Stmt)));
         New_Line;
      end if;

      Port := Get_Port (Conn);
      if Get_Kind (Port) = N_Port then
         Port := Get_Expression (Port);
         pragma Assert (Get_Kind (Port) = N_Name);
         Port := Get_Declaration (Port);
      end if;
      pragma Assert (Get_Kind (Port) = N_Output);
      Port := Get_Redeclaration (Port);

      declare
         Port_Type : constant Node := Get_Type_Data_Type (Port);
         Ssize : constant Storage_Index := Get_Storage_Size (Port_Type);
         Res : Storage_Type (0 .. Ssize - 1);
      begin
         Execute_Expression (null, Res'Address, Port);

         if Flag_Trace then
            Trace_Head;
            Disp_Verilog.Disp_Expression (Port);
            Trace (" >>> ");
            Allocates.Disp_Value (Res'Address, Port_Type);
            Trace_Newline;
         end if;

         Blocking_Assign_Lvalue (null, Expr, Res'Address, Port_Type);
      end;
   end Execute_Conn_Output;

   procedure Execute_Conn_Default (Proc : Process_Acc)
   is
      Conn : constant Node := Proc.Stmt;
      Port : constant Node := Get_Port (Conn);
      Expr : constant Node := Get_Default_Value (Port);
      Decl : Node;
   begin
      if Flag_Trace_Exec or Flag_Trace then
         Put ("execute conn default at ");
         Put (Files_Map.Image (Get_Location (Proc.Stmt)));
         New_Line;
      end if;

      Decl := Get_Redeclaration (Port);

      Execute_Implicit_Assign (null, Decl, Expr);
   end Execute_Conn_Default;

   function Get_Current_Time return Uns32 is
   begin
      return Time;
   end Get_Current_Time;

   --  Schedule processes that depends on explicitly initialized variables.
   --  (this is done by re-assigning variables).
   procedure Initialize_Static_Var (Frame : Frame_Ptr; Var : Node)
   is
      Expr : constant Node := Get_Expression (Var);
   begin
      if Expr = Null_Node then
         return;
      end if;

      Execute_Implicit_Assign (Frame, Var, Expr);
   end Initialize_Static_Var;

   procedure Initialize_Static_Var_Chain (Frame : Frame_Ptr; First : Node)
   is
      Item : Node;
   begin
      Item := First;
      while Item /= Null_Node loop
         case Get_Kind (Item) is
            when Nkinds_Nets
              | Nkinds_Net_Port
              | Nkinds_Tf_Port
              | Nkinds_Gate
              | N_Parameter
              | N_Localparam
              | Nkinds_Forward_Typedef
              | N_Typedef
              | N_Constraint =>
               null;
            when N_Function
              | N_Task =>
               --  FIXME: what about static var ?
               if Get_Lifetime (Item) = Life_Static then
                  Initialize_Static_Var_Chain
                    (Frame, Get_Tf_Item_Declaration_Chain (Item));
                  Initialize_Static_Var_Chain
                    (Frame, Get_Statements_Chain (Item));
               end if;
            when N_Extern_Function
              | N_Extern_Task =>
               null;
            when N_OOB_Function
              | N_OOB_Task =>
               null;
            when N_Assign
              | N_Return_Stmt
              | N_Post_Increment =>
               null;
            when N_Var
              | N_Foreach_Variable =>
               if not Get_Is_Automatic (Item) then
                  Initialize_Static_Var (Frame, Item);
               end if;
            when N_Class =>
               if not Get_Virtual_Flag (Item) then
                  Initialize_Static_Var_Chain
                    (Frame, Get_Class_Item_Chain (Item));
               end if;
            when N_Always
              | N_Always_Comb
              | N_Initial =>
               Initialize_Static_Var_Chain (Frame, Get_Statement (Item));
            when N_Seq_Block
              | N_Par_Block =>
               Initialize_Static_Var_Chain
                 (Frame, Get_Block_Item_Declaration_Chain (Item));
               Initialize_Static_Var_Chain
                 (Frame, Get_Statements_Chain (Item));
            when N_Simple_Immediate_Assert =>
               Initialize_Static_Var_Chain (Frame, Get_Pass_Stmt (Item));
               Initialize_Static_Var_Chain (Frame, Get_Else_Stmt (Item));
            when N_If =>
               Initialize_Static_Var_Chain (Frame, Get_True_Stmt (Item));
               Initialize_Static_Var_Chain (Frame, Get_False_Stmt (Item));
            when N_Delay_Control
              | N_Event_Control =>
               Initialize_Static_Var_Chain (Frame, Get_Statement (Item));
            when N_Repeat_Control =>
               Initialize_Static_Var_Chain (Frame, Get_Statement (Item));
            when N_Subroutine_Call_Stmt
              | N_Blocking_Assign
              | N_Noblk_Assign
              | N_Break_Stmt
              | N_Continue_Stmt
              | N_Assign_Operator
              | N_Unpack_Assign
              | N_Pack_Assign
              | N_Post_Decrement
              | N_Pre_Decrement
              | N_Trigger
              | N_Wait
              | N_Wait_Fork
              | N_Disable_Fork =>
               null;
            when Nkinds_Case =>
               Initialize_Static_Var_Chain (Frame, Get_Case_Items (Item));
            when N_Case_Item
              | N_Default_Case_Item
              | N_For
              | N_Do_While
              | N_While
              | N_Forever
              | N_Repeat =>
               Initialize_Static_Var_Chain (Frame, Get_Statement (Item));
            when N_Foreach =>
               Initialize_Static_Var_Chain
                 (Frame, Get_Foreach_Variables (Item));
               Initialize_Static_Var_Chain (Frame, Get_Statement (Item));
            when N_Module_Instance
              | N_Program_Instance =>
               declare
                  Module : constant Node := Get_Instance (Item);
               begin
                  Initialize_Static_Var_Chain
                    (Frame, Get_Items_Chain (Module));
               end;
            when N_Interface_Declaration =>
               --  Nothing to do.
               null;
            when N_Interface_Instance =>
               declare
                  Inter : constant Node :=
                    Get_Declaration (Get_Interface_Name (Item));
                  Iframe : constant Frame_Ptr := Get_Sub_Frame (Frame, Item);
               begin
                  Initialize_Static_Var_Chain
                    (Iframe, Get_Items_Chain (Inter));
               end;
            when N_Compilation_Unit =>
               Initialize_Static_Var_Chain (Frame, Get_Descriptions (Item));
            when N_Package =>
               Initialize_Static_Var_Chain
                 (Frame, Get_Package_Item_Chain (Item));
            when N_Module
              | N_Program_Declaration =>
               null;
            when N_Generic_Class =>
               null;
            when N_Genvar
              | N_Loop_Generate
              | N_If_Generate =>
               null;
            when N_Generate_Region
              | N_Generate_Block
              | N_Array_Generate_Block
              | N_Indexed_Generate_Block =>
               Initialize_Static_Var_Chain
                 (Frame, Get_Generate_Item_Chain (Item));
            when N_Package_Import =>
               null;
            when N_Assert_Property =>
               --  TODO
               null;
            when N_Import_DPI_Function
              | N_Export_DPI_Function =>
               null;
            when others =>
               Error_Kind ("initialize_static_var_chain", Item);
         end case;
         Item := Get_Chain (Item);
      end loop;
   end Initialize_Static_Var_Chain;

   function Find_Next_Time return Boolean
   is
      T1 : Uns32;
      L1 : Boolean;
      L2 : Boolean;
   begin
      L1 := False;
      L2 := False;
      loop
         Time := Time + 1;
         T1 := Time mod Time_Wheel_Size;

         if T1 = 0 then
            --  End of time wheel.
            --  The time wheel may not be empty, because there might be events
            --  for the near future recently scheduled; but it also needs to
            --  be refilled from time wheel 2 where there can be events now
            --  for the near future that have been scheduled a long time ago.
            loop
               declare
                  T2 : constant Uns32 := Time / Time_Wheel_Size;
                  Idx : constant Uns32 := T2 mod Time_Wheel_Size;
               begin
                  if Idx = 0 then
                     --  End of time wheel 2.  Need to refill from time
                     --  wheel 3.
                     if not Is_Empty (Time_Wheel3.Nba)
                       or else not Is_Empty (Time_Wheel3.Inactive)
                     then
                        --  TODO: extract all events in time wheel 3 and
                        --  move them to time wheel 2 (or keep in wheel 3).
                        L2 := False;
                        declare
                           P, Next_P : Process_Acc;
                           Slot : Uns32;
                        begin
                           P := Time_Wheel3.Inactive.First;
                           Time_Wheel3.Inactive := (null, null);
                           while P /= null loop
                              Next_P := P.Enabled_Chain;
                              P.Enabled_Chain := null;
                              pragma Assert (Time mod Time_Wheel_Size = 0);
                              Slot := (P.Wakeup - Time) / Time_Wheel_Size;
                              if Slot < Time_Wheel_Size then
                                 Append_Queue (Time_Wheel2 (Slot).Inactive, P);
                              else
                                 Append_Queue (Time_Wheel3.Inactive, P);
                              end if;
                              P := Next_P;
                           end loop;
                        end;
                        --  TODO: for NBA.
                        pragma Assert (Is_Empty (Time_Wheel3.Nba));
                     else
                        if L2 and L1 then
                           --  No more events.
                           return False;
                        else
                           L2 := True;
                        end if;
                     end if;
                  end if;
                  if not Is_Empty (Time_Wheel2 (Idx).Nba)
                    or else not Is_Empty (Time_Wheel2 (Idx).Inactive)
                  then
                     --  Extract all events in time wheel 2 (idx) and
                     --  move them to time wheel 1.
                     L1 := False;
                     declare
                        P, Next_P : Process_Acc;
                        Slot : Uns32;
                     begin
                        P := Time_Wheel2 (Idx).Inactive.First;
                        while P /= null loop
                           Next_P := P.Enabled_Chain;
                           P.Enabled_Chain := null;
                           pragma Assert (Time mod Time_Wheel_Size = 0);
                           Slot := P.Wakeup mod Time_Wheel_Size;
                           Append_Queue (Time_Wheel (Slot).Inactive, P);
                           P := Next_P;
                        end loop;
                        Time_Wheel2 (Idx).Inactive := (null, null);
                     end;
                     --  TODO: for NBA.
                     pragma Assert (Is_Empty (Time_Wheel2 (Idx).Nba));
                     exit;
                  else
                     if L1 then
                        --  Time wheel 1 is empty.
                        Time := Time + Time_Wheel_Size;
                     else
                        --  Time wheel 1 has not been refilled.  Continue to
                        --  scan it; at the next refill we know it is empty.
                        L1 := True;
                        exit;
                     end if;
                  end if;
               end;
            end loop;
         end if;

         Current_Entry := Time_Wheel (T1)'Access;
         if not Is_Empty (Current_Entry.Inactive)
           or else not Is_Empty (Current_Entry.Nba)
         then
            return True;
         end if;
      end loop;
      --  return False;
   end Find_Next_Time;

   procedure Run (Root : Node)
   is
      First : Process_Acc;
      Proc : Process_Acc;

      Events, Event : Event_Acc;
   begin
      Time := 0;
      Current_Entry := Time_Wheel (0)'Access;

      --  1800-2017 6.8 Variable declarations
      --  Setting the initial value of a static variable as part of the
      --  variable declaration (including static class members) shall occur
      --  before any initial or always procedures are started.

      Initialize_Static_Var_Chain (Global_Frame, Elaborate.Units_Chain);
      Initialize_Static_Var_Chain (Global_Frame, Get_Items_Chain (Root));

      --  Variables at the beginning of simulation.
      if Flag_Trace_Values then
         Disp_All_Vars (Flag_Trace_Memories);
      end if;

      --  Main loop.
      loop
         --  Active events.
         loop
            --  Get list of enabled processes.
            Extract_All (Active_Procs, First);

            while First /= null loop
               --  Extract Proc from the list.
               Proc := First;
               First := First.Enabled_Chain;
               Proc.Enabled_Chain := null;
               Proc.Is_Scheduled := False;

               case Proc.Kind is
                  when Process_User =>
                     Execute_Process (Proc);
                  when Process_Assign =>
                     Execute_Assign (Proc);
                  when Process_Implicit_Assign =>
                     Execute_Implicit_Assign (Proc);
                  when Process_Gate =>
                     Execute_Gate (Proc);
                  when Process_Conn_Input =>
                     Execute_Conn_Input (Proc);
                  when Process_Conn_Output =>
                     Execute_Conn_Output (Proc);
                  when Process_Conn_Default =>
                     Execute_Conn_Default (Proc);
               end case;

               Free_Unreferenced_Process (Proc);

               if Verilog.Vpi.Vpip_Control /= 0 then
                  if Verilog.Vpi.Vpip_Control = Natural (Verilog.Vpi.VpiStop)
                  then
                     --  $stop occured; cancel it.
                     Verilog.Vpi.Vpip_Control := 0;

                     Verilog.Debugger.Debug_CLI;
                  else
                     return;
                  end if;
               end if;
            end loop;
            exit when Is_Empty (Active_Procs);
         end loop;

         if not Is_Empty (Current_Entry.Inactive) then
            --  Events (only processes) in inactive
            Active_Procs := Current_Entry.Inactive;
            Current_Entry.Inactive := (null, null);
         elsif not Is_Empty (Current_Entry.Nba) then
            --  NBA region
            Extract_All (Current_Entry.Nba, Events);

            while Events /= null loop
               Event := Events;
               case Event.Kind is
                  when Nba_Vector_Event =>
                     Assign_Vector (Event.Target_Data,
                                    Event.Target_Offset,
                                    Event.Target_Width,
                                    Event.Target_Type,
                                    Event.Target_Update,
                                    Event.Value'Address,
                                    Event.Target_Doffset);
                  when Nba_Nonvec_Event =>
                     Assign_Nonvec (Event.Target_Data, Event.Target_Type,
                                    Event.Target_Update, Event.Value'Address,
                                    Event.Target_Type, Null_Node);
                  when others =>
                     raise Program_Error;
               end case;
               Events := Event.Next;
               Free (Event);
            end loop;
         else
            if Flag_Trace_Time or Flag_Trace then
               Put_Line ("#" & Uns32'Image (Time));
            end if;
            if Flag_Trace_Values then
               Disp_All_Vars (Flag_Trace_Memories);
            end if;

            Vpi.Execute_Read_Only_Synch_Cb;

            --  Find next time.
            pragma Assert (Current_Entry.Inactive = (null, null));
            pragma Assert (Current_Entry.Nba = (null, null));

            exit when not Find_Next_Time;
         end if;
      end loop;
   end Run;
end Verilog.Simulation;
