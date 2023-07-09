--  Object layout for verilog
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

with Simple_IO; use Simple_IO;
with Utils_IO; use Utils_IO;
with Tables;
with Name_Table;
with Files_Map;
with Verilog.Nutils; use Verilog.Nutils;
with Verilog.Sem_Utils; use Verilog.Sem_Utils;
with Verilog.Sem_Types;
with Verilog.Errors; use Verilog.Errors;
with Verilog.Sv_Strings; use Verilog.Sv_Strings;
with Verilog.Sv_Arrays; use Verilog.Sv_Arrays;
with Verilog.Sv_Queues; use Verilog.Sv_Queues;
with Verilog.Sv_Maps; use Verilog.Sv_Maps;
with Verilog.Simulation; use Verilog.Simulation;
with Verilog.Executions; use Verilog.Executions;
with Verilog.Abi;
with Verilog.Sv_Classes; use Verilog.Sv_Classes;

package body Verilog.Allocates is
   type Update_Acc_Acc is access all Update_Acc;
   function To_Update_Acc_Acc is
      new Ada.Unchecked_Conversion (Data_Ptr, Update_Acc_Acc);

   type Sensitivity_El (Kind : Update_Kind) is record
      case Kind is
         when Update_Process =>
            Proc : Process_Acc;
            Pc : Node;
         when Update_Edge =>
            Edge : Edge_Process_Acc;
         when Update_Vpi =>
            null;
      end case;
   end record;

   --  Add EXPR in sensitivity list of PROC.
   procedure Add_Updates
     (Expr : Node; Sens : Sensitivity_El; Is_Lvalue : Boolean := False);

   type Scope_Kind is (Scope_Global, Scope_Tf,
                       Scope_Struct, Scope_Class, Scope_Interface);
   type Obj_Kind is (Obj_Static_Var, Obj_Frame_Var, Obj_Member,
                     Obj_Interface_Instance);

   type Obj_Type (Kind : Obj_Kind);
   type Obj_Acc is access Obj_Type;

   type Scope_Type (Kind : Scope_Kind) is record
      First, Last : Obj_Acc;

      Size : Storage_Index;
      Align : Storage_Index;
   end record;

   type Scope_Acc is access Scope_Type;

   Global_Scope : Scope_Acc;

   type Obj_Type (Kind : Obj_Kind) is record
      Scope : Scope_Acc;
      Next : Obj_Acc;
      Decl : Node;
      Obj_Off : Storage_Index;
      case Kind is
         when Obj_Frame_Var
           | Obj_Static_Var =>
            Upd_Off : Storage_Index;
         when Obj_Member =>
            null;
         when Obj_Interface_Instance =>
            Decl_Scope : Scope_Acc;
      end case;
   end record;

   package Objs is new Tables
     (Table_Component_Type => Obj_Acc,
      Table_Index_Type => Obj_Id,
      Table_Low_Bound => No_Obj_Id + 1,
      Table_Initial => 32);

   package Scopes is new Tables
     (Table_Component_Type => Scope_Acc,
      Table_Index_Type => Scope_Id,
      Table_Low_Bound => No_Scope_Id + 1,
      Table_Initial => 8);

   procedure Scope_Append (Scope : Scope_Acc; Obj : Obj_Acc) is
   begin
      if Scope.First = null then
         Scope.First := Obj;
      else
         Scope.Last.Next := Obj;
      end if;
      Scope.Last := Obj;
      pragma Assert (Obj.Next = null);
   end Scope_Append;

   function Get_Storage_Size (Typ : Node) return Storage_Index is
   begin
      case Get_Kind (Typ) is
         when N_Void_Type =>
            return 0;
         when N_Logic_Type =>
            return 1;
         when N_Bit_Type =>
            return 1;
         when N_Log_Packed_Array_Cst =>
            declare
               W : constant Width_Type := Get_Type_Width (Typ);
               --  Round to Digit_Width.
               Len : constant Storage_Index :=
                 Storage_Index ((W + Digit_Width - 1) / Digit_Width);
               El_Size : constant Storage_Index :=
                 Storage_Index (Logic_32'Size / 8);
            begin
               return El_Size * Len;
            end;
         when N_Bit_Packed_Array_Cst =>
            declare
               W : constant Width_Type := Get_Type_Width (Typ);
               --  Round to Digit_Width.
               Len : constant Storage_Index :=
                 Storage_Index ((W + Digit_Width - 1) / Digit_Width);
               El_Size : constant Storage_Index :=
                 Storage_Index (Uns32'Size / 8);
            begin
               return El_Size * Len;
            end;
         when N_Real_Type =>
            return Verilog.Abi.Real_Size;
         when N_Shortreal_Type =>
            return Verilog.Abi.Shortreal_Size;
         when N_Array_Cst
           | N_Struct_Type =>
            return Storage_Index (Get_Type_Size (Typ));
         when N_Enum_Type =>
            return Get_Storage_Size (Get_Enum_Base_Type (Typ));
         when N_Packed_Struct_Type =>
            return Get_Storage_Size (Get_Packed_Base_Type (Typ));
         when N_String_Type =>
            return Verilog.Abi.Sv_String_Size;
         when Nkinds_Class =>
            return Verilog.Abi.Ptr_Size;
         when N_Dynamic_Array_Cst
           | N_Queue_Cst
           | N_Associative_Array_Cst =>
            return Verilog.Abi.Ptr_Size;
         when N_Event_Type =>
            return Verilog.Abi.Ptr_Size;
         when others =>
            Error_Kind ("get_storage_size", Typ);
      end case;
   end Get_Storage_Size;

   function Get_Storage_Align (Typ : Node) return Storage_Index is
   begin
      case Get_Kind (Typ) is
         when N_Logic_Type
            | N_Bit_Type =>
            return 1;
         when N_Event_Type =>
            return Verilog.Abi.Ptr_Align;
         when N_Real_Type =>
            return Verilog.Abi.Real_Align;
         when N_Shortreal_Type =>
            return Verilog.Abi.Shortreal_Align;
         when N_Log_Packed_Array_Cst =>
            return Logic_32'Alignment;
         when N_Bit_Packed_Array_Cst =>
            return Uns32'Alignment;
         when N_Array_Cst =>
            return Get_Storage_Align (Get_Type_Element_Type (Typ));
         when N_Enum_Type =>
            return Get_Storage_Align (Get_Enum_Base_Type (Typ));
         when N_Packed_Struct_Type =>
            return Get_Storage_Align (Get_Packed_Base_Type (Typ));
         when N_String_Type =>
            return Verilog.Abi.Sv_String_Align;
         when Nkinds_Class =>
            return Verilog.Abi.Ptr_Align;
         when N_Struct_Type =>
            declare
               Scope : constant Scope_Acc := Scopes.Table (Get_Scope_Id (Typ));
            begin
               return Scope.Align;
            end;
         when N_Dynamic_Array_Cst
           | N_Queue_Cst
           | N_Associative_Array_Cst =>
            return Verilog.Abi.Ptr_Align;
         when others =>
            Error_Kind ("get_storage_align", Typ);
      end case;
   end Get_Storage_Align;

   function Get_Obj_Acc (Var : Node) return Obj_Acc is
   begin
      return Objs.Table (Get_Obj_Id (Var));
   end Get_Obj_Acc;

   function Get_Var_Data (Frame : Frame_Ptr; Var : Node) return Data_Ptr
   is
      Obj : constant Obj_Acc := Get_Obj_Acc (Var);
      F : Frame_Ptr;
   begin
      case Obj.Kind is
         when Obj_Static_Var
           | Obj_Interface_Instance =>
            F := Global_Frame;
         when Obj_Frame_Var
           | Obj_Member =>
            pragma Assert (Frame /= null);
            F := Frame;
      end case;
      return F (Obj.Obj_Off)'Address;
   end Get_Var_Data;

   function Get_Var_Update_Acc (Var : Node) return Update_Acc_Acc
   is
      Obj : constant Obj_Acc := Get_Obj_Acc (Var);
   begin
      pragma Assert (not Get_Is_Automatic (Var));
      return To_Update_Acc_Acc (Global_Frame (Obj.Upd_Off)'Address);
   end Get_Var_Update_Acc;

   function Get_Var_Update (Var : Node) return Update_Acc is
   begin
      return Get_Var_Update_Acc (Var).all;
   end Get_Var_Update;

   procedure Set_Var_Update (Var : Node; Upd : Update_Acc) is
   begin
      Get_Var_Update_Acc (Var).all := Upd;
   end Set_Var_Update;

   function Get_Sub_Frame (Frame : Frame_Ptr; Obj : Obj_Acc)
                          return Frame_Ptr is
   begin
      return To_Frame_Ptr (Frame (Obj.Obj_Off)'Address);
   end Get_Sub_Frame;

   function Get_Sub_Frame (Frame : Frame_Ptr; N : Node) return Frame_Ptr
   is
      pragma Assert (Nkind_In (Get_Kind (N),
                               N_Interface_Instance, N_Interface_Port));
      Obj : constant Obj_Acc := Objs.Table (Get_Obj_Id (N));
   begin
      return Get_Sub_Frame (Frame, Obj);
   end Get_Sub_Frame;

   function Get_Unpacked_Member_Offset (Member : Node) return Storage_Index
   is
      Obj : constant Obj_Acc := Objs.Table (Get_Obj_Id (Member));
   begin
      return Obj.Obj_Off;
   end Get_Unpacked_Member_Offset;

   function Get_Scope (N : Node) return Scope_Acc
   is
      Id : constant Scope_Id := Get_Scope_Id (N);
   begin
      return Scopes.Table (Id);
   end Get_Scope;

   --  1800-2017 6.8 Variable declarations
   --  See table 6.7
   procedure Init (Dest : Data_Ptr; Atype : Node) is
   begin
      case Get_Kind (Atype) is
         when N_Log_Packed_Array_Cst =>
            declare
               W : constant Width_Type := Get_Type_Width (Atype);
               Last : constant Digit_Index := To_Last (W);
               D : constant Logvec_Ptr := To_Logvec_Ptr (Dest);
            begin
               D (0 .. Last) := (others => (Val => not 0, Zx => not 0));
            end;
         when N_Bit_Packed_Array_Cst =>
            declare
               W : constant Width_Type := Get_Type_Width (Atype);
               Last : constant Digit_Index := To_Last (W);
               D : constant Bitvec_Ptr := To_Bitvec_Ptr (Dest);
            begin
               D (0 .. Last) := (others => 0);
            end;
         when N_Logic_Type =>
            To_Logic_Ptr (Dest).all := V_X;
         when N_Bit_Type =>
            To_Bit_Ptr (Dest).all := B_0;
         when N_Shortreal_Type =>
            To_Fp32_Ptr (Dest).all := 0.0;
         when N_Real_Type =>
            To_Fp64_Ptr (Dest).all := 0.0;
         when N_Array_Cst =>
            declare
               El_Type : constant Node := Get_Type_Element_Type (Atype);
               Stride : constant Storage_Index := Get_Storage_Size (El_Type);
            begin
               for I in 1 .. Compute_Length (Atype) loop
                  Init (Dest + (Stride * Storage_Index (I - 1)), El_Type);
               end loop;
            end;
         when N_String_Type =>
            Ref (Empty_Sv_String);
            To_Sv_String_Ptr (Dest).all := Empty_Sv_String;
         when N_Enum_Type =>
            Init (Dest, Get_Enum_Base_Type (Atype));
         when N_Packed_Struct_Type =>
            --  TODO: default value ?
            Init (Dest, Get_Packed_Base_Type (Atype));
         when N_Struct_Type =>
            declare
               Member : Node;
               Obj : Obj_Acc;
            begin
               Member := Get_Members (Atype);
               while Member /= Null_Node loop
                  Obj := Objs.Table (Get_Obj_Id (Member));
                  Init (Dest + Obj.Obj_Off, Get_Type_Data_Type (Member));
                  Member := Get_Chain (Member);
               end loop;
            end;
         when Nkinds_Class =>
            To_Sv_Class_Ptr (Dest).all := null;
         when N_Dynamic_Array_Cst =>
            To_Sv_Dyn_Array_Ptr_Ptr (Dest).all := null;
         when N_Queue_Cst =>
            declare
               El_Typ : constant Node := Get_Type_Element_Type (Atype);
               Max : constant Int32 := Get_Maximum_Size_Cst (Atype);
               Lim : Uns32;
            begin
               if Max = -1 then
                  Lim := Unlimited;
               else
                  Lim := Uns32 (Max);
               end if;
               To_Sv_Queue_Ptr (Dest).all :=
                 Queue_New (Get_Storage_Size (El_Typ), Lim, 0);
            end;
         when N_Associative_Array_Cst =>
            To_Sv_Map_Ptr (Dest).all := New_Sv_Map (Atype);
         when others =>
            Error_Kind ("init", Atype);
      end case;
   end Init;

   procedure Clear_Var (Frame : Frame_Ptr; Decl : Node; Decl_Type : Node) is
   begin
      Init (Get_Var_Data (Frame, Decl), Decl_Type);
   end Clear_Var;

   procedure Init_Var (Frame : Frame_Ptr; Decl : Node)
   is
      Expr : constant Node := Get_Expression (Decl);
   begin
      if Expr /= Null_Node then
         Execute_Expression (Frame, Get_Var_Data (Frame, Decl), Expr);
      else
         Init (Get_Var_Data (Frame, Decl), Get_Type_Data_Type (Decl));
      end if;
   end Init_Var;

   procedure Init_Scope (Frame : Frame_Ptr; First_Obj : Obj_Acc)
   is
      Obj : Obj_Acc;
      Decl : Node;
   begin
      Obj := First_Obj;
      while Obj /= null loop
         Decl := Obj.Decl;
         case Get_Kind (Decl) is
            when N_Var
              | Nkinds_Tf_Port =>
               Init (Get_Var_Data (Frame, Decl), Get_Type_Data_Type (Decl));
               --  Init_Var (null, Decl);
               if not Get_Is_Automatic (Decl) then
                  Set_Var_Update (Decl, null);
               end if;
            when N_Return_Var =>
               Init (Get_Var_Data (null, Decl), Get_Expr_Type (Decl));
               if not Get_Is_Automatic (Decl) then
                  Set_Var_Update (Decl, null);
               end if;
            when N_This_Var =>
               null;
            when N_Wire
              | N_Wire_Direct =>
               Init (Get_Var_Data (Frame, Decl), Get_Type_Data_Type (Decl));
               Set_Var_Update (Decl, null);
            when N_Parameter =>
               raise Internal_Error;
            when N_Localparam =>
               raise Internal_Error;
            when N_Repeat =>
               null;
            when N_Interface_Instance =>
               Init_Scope (To_Frame_Ptr (Frame (Obj.Obj_Off)'Address),
                           Obj.Decl_Scope.First);
            when others =>
               Error_Kind ("init_scope", Decl);
         end case;
         Obj := Obj.Next;
      end loop;
   end Init_Scope;

   procedure Init_Class_Scope (Cls : Node; Frame : Frame_Ptr)
   is
      Scope : constant Scope_Acc := Get_Scope (Cls);
      Obj : Obj_Acc;
   begin
      Obj := Scope.First;
      while Obj /= null loop
         Init_Var (Frame, Obj.Decl);
         Obj := Obj.Next;
      end loop;
   end Init_Class_Scope;

   function Align_Storage_Size (Size : Storage_Index; Align : Storage_Index)
                               return Storage_Index is
   begin
      return (Size + Align - 1) and not (Align - 1);
   end Align_Storage_Size;

   procedure Align_Scope_Size (Scope : Scope_Acc; Align : Storage_Index) is
   begin
      Scope.Size := Align_Storage_Size (Scope.Size, Align);
   end Align_Scope_Size;

   procedure Allocate_Data_Raw (Scope : Scope_Acc;
                                Offset : out Storage_Index;
                                Size : Storage_Index;
                                Align : Storage_Index) is
   begin
      Align_Scope_Size (Scope, Align);
      Scope.Align := Storage_Index'Max (Scope.Align, Align);
      Offset := Scope.Size;
      Scope.Size := Scope.Size + Size;
   end Allocate_Data_Raw;

   procedure Allocate_Data_By_Type (Scope : Scope_Acc;
                                    Offset : out Storage_Index;
                                    Typ : Node) is
   begin
      Allocate_Data_Raw
        (Scope, Offset, Get_Storage_Size (Typ), Get_Storage_Align (Typ));
   end Allocate_Data_By_Type;

   --  Allocate resources for variable/net.
   procedure Allocate_Var (Frame_Scope : Scope_Acc; Var : Node; Vtype : Node)
   is
      Id : Obj_Id;
      Obj : Obj_Acc;
      Is_Frame : Boolean;
      Scope : Scope_Acc;
   begin
      Id := Get_Obj_Id (Var);
      --  Might be an alias of an already connected net.
      if Id /= No_Obj_Id then
         --  FIXME: improve the check.
         return;
      end if;

      case Frame_Scope.Kind is
         when Scope_Global
           | Scope_Tf
           | Scope_Class =>
            if Get_Is_Automatic (Var) then
               Scope := Frame_Scope;
               Is_Frame := True;
            else
               Scope := Global_Scope;
               Is_Frame := False;
            end if;
         when Scope_Struct
           | Scope_Interface =>
            Scope := Frame_Scope;
            Is_Frame := True;
      end case;

      if Is_Frame then
         Obj := new Obj_Type'(Kind => Obj_Frame_Var,
                              Scope => Scope,
                              Decl => Var,
                              Next => null,
                              Obj_Off => 0,
                              Upd_Off => 0);
      else
         Obj := new Obj_Type'(Kind => Obj_Static_Var,
                              Scope => Global_Scope,
                              Decl => Var,
                              Next => null,
                              Obj_Off => 0,
                              Upd_Off => 0);
      end if;
      Allocate_Data_By_Type (Scope, Obj.Obj_Off, Vtype);

      if not Get_Is_Automatic (Var) then
         Allocate_Data_Raw
           (Scope, Obj.Upd_Off, Process_Acc'Size / 8, Process_Acc'Alignment);
      end if;
      Scope_Append (Scope, Obj);

      Objs.Append (Obj);
      Id := Objs.Last;
      Set_Obj_Id (Var, Id);
   end Allocate_Var;

   procedure Allocate_Interface_Instance (Scope : Scope_Acc; Inst : Node)
   is
      Decl : constant Node := Get_Declaration (Get_Interface_Name (Inst));
      Decl_Scope : constant Scope_Acc := Scopes.Table (Get_Scope_Id (Decl));
      Id : Obj_Id;
      Obj : Obj_Acc;
   begin
      Obj := new Obj_Type'(Kind => Obj_Interface_Instance,
                           Scope => Scope,
                           Decl => Inst,
                           Next => null,
                           Obj_Off => 0,
                           Decl_Scope => Decl_Scope);

      Allocate_Data_Raw
        (Scope, Obj.Obj_Off, Decl_Scope.Size, Decl_Scope.Align);

      Objs.Append (Obj);
      Id := Objs.Last;
      Set_Obj_Id (Inst, Id);

      Scope_Append (Scope, Obj);
   end Allocate_Interface_Instance;

   procedure Allocate_Member (Scope : Scope_Acc; Member : Node)
   is
      Vtype : constant Node := Get_Type_Data_Type (Member);
      Id : Obj_Id;
      Obj : Obj_Acc;
   begin
      Id := Get_Obj_Id (Member);
      pragma Assert (Id = No_Obj_Id);

      Obj := new Obj_Type'(Kind => Obj_Member,
                           Scope => Scope,
                           Decl => Member,
                           Next => null,
                           Obj_Off => 0);

      Allocate_Data_By_Type (Scope, Obj.Obj_Off, Vtype);

      Objs.Append (Obj);
      Id := Objs.Last;
      Set_Obj_Id (Member, Id);

      Scope_Append (Scope, Obj);
   end Allocate_Member;

   procedure Allocate_Collapsed_Port (Loconn : Node; Expr : Node)
   is
      Loconn_Decl : Node;
      Decl : Node;
   begin
      pragma Assert (Get_Kind (Loconn) in Nkinds_Net_Port);
      Loconn_Decl := Get_Redeclaration (Loconn);
      pragma Assert (Loconn_Decl /= Null_Node);

      case Get_Kind (Expr) is
         when N_Name =>
            --  Nkinds_Nets or N_Var ...
            --  Rename the loconn (ie merge declaration).
            Decl := Get_Declaration (Expr);
            if Get_Kind (Decl) in Nkinds_Net_Port then
               Decl := Get_Redeclaration (Decl);
            end if;
            Set_Obj_Id (Loconn_Decl, Get_Obj_Id (Decl));
         when others =>
            raise Internal_Error;
      end case;
   end Allocate_Collapsed_Port;

   procedure Allocate_Collapsed_Connections (Scope : Scope_Acc; Conns : Node)
   is
      pragma Unreferenced (Scope);
      Conn : Node;
      Port : Node;
      Expr : Node;
      Loconn : Node;
      Collapse : Boolean;
   begin
      Conn := Conns;
      while Conn /= Null_Node loop
         case Nkinds_Connection (Get_Kind (Conn)) is
            when N_Port_Connection =>
               Collapse := Get_Collapse_Flag (Conn);
            when others =>
               Collapse := False;
         end case;
         if Collapse then
            Port := Get_Port (Conn);
            Expr := Get_Expression (Conn);
            pragma Assert (Expr /= Null_Node);

            case Get_Kind (Port) is
               when N_Port =>
                  --  The port must be a simple port_expression (only a name).
                  Loconn := Get_Expression (Port);
                  pragma Assert (Get_Kind (Loconn) = N_Name);
                  Loconn := Get_Declaration (Loconn);
                  Allocate_Collapsed_Port (Loconn, Expr);
               when Nkinds_Net_Port =>
                  Allocate_Collapsed_Port (Port, Expr);
               when N_Interface_Port =>
                  declare
                     Decl : constant Node := Get_Declaration (Expr);
                  begin
                     Set_Obj_Id (Port, Get_Obj_Id (Decl));
                  end;
               when N_Modport_Port =>
                  declare
                     Inter : constant Node :=
                       Get_Declaration (Get_Name (Expr));
                  begin
                     Set_Obj_Id (Port, Get_Obj_Id (Inter));
                  end;
               when others =>
                  Error_Kind ("allocate_collapsed_connections", Port);
            end case;
         end if;

         Conn := Get_Chain (Conn);
      end loop;
   end Allocate_Collapsed_Connections;

   procedure Allocate_Connections_Process (Scope : Scope_Acc; Conns : Node)
   is
      pragma Unreferenced (Scope);
      Conn : Node;
      Port : Node;
      Expr : Node;
      Loconn : Node;
      Proc : Process_Acc;
      Sens : Sensitivity_El (Update_Process);
   begin
      Conn := Conns;
      while Conn /= Null_Node loop
         Expr := Null_Node;
         case Nkinds_Connection (Get_Kind (Conn)) is
            when N_Port_Connection
              | N_Implicit_Connection =>
               if not Get_Collapse_Flag (Conn) then
                  Expr := Get_Expression (Conn);
                  Port := Get_Port (Conn);
               end if;
            when N_Default_Connection =>
               Port := Get_Port (Conn);
               Expr := Get_Default_Value (Port);
            when N_Wildcard_Connection =>
               null;
         end case;

         if Expr /= Null_Node then
            --  The port must be a simple port_expression (only a name).
            if Get_Kind (Port) = N_Port then
               Loconn := Get_Expression (Port);
               pragma Assert (Get_Kind (Loconn) = N_Name);
               Loconn := Get_Declaration (Loconn);
            else
               Loconn := Port;
            end if;
            --  TODO: concat, element.
            pragma Assert (Get_Kind (Loconn) in Nkinds_Net_Port);

            case Nkinds_Net_Port (Get_Kind (Loconn)) is
               when N_Input =>
                  if Get_Kind (Conn) = N_Default_Connection then
                     Proc := new Process_Type'(Kind => Process_Conn_Default,
                                               Is_Scheduled => False,
                                               Stmt => Conn,
                                               Wakeup => 0,
                                               Nref => 0,
                                               Enabled_Chain => null);
                  else
                     Proc := new Process_Type'(Kind => Process_Conn_Input,
                                               Is_Scheduled => False,
                                               Stmt => Conn,
                                               Wakeup => 0,
                                               Nref => 0,
                                               Enabled_Chain => null);
                     Sens := (Kind => Update_Process,
                              Proc => Proc,
                              Pc => Null_Node);
                     Add_Updates (Expr, Sens);
                  end if;
                  Activate_Process (Proc);
               when N_Output =>
                  Proc := new Process_Type'(Kind => Process_Conn_Output,
                                            Is_Scheduled => False,
                                            Stmt => Conn,
                                            Wakeup => 0,
                                            Nref => 0,
                                            Enabled_Chain => null);
                  Sens := (Kind => Update_Process,
                           Proc => Proc,
                           Pc => Null_Node);

                  Loconn := Get_Redeclaration (Loconn);
                  pragma Assert (Loconn /= Null_Node);

                  Add_Updates (Loconn, Sens);
                  Activate_Process (Proc);
               when N_Inout =>
                  raise Internal_Error;
            end case;
         end if;

         Conn := Get_Chain (Conn);
      end loop;
   end Allocate_Connections_Process;

   function Allocate_Frame (Scope : Scope_Acc) return Frame_Ptr is
   begin
      if Scope.Size = 0 then
         return Null_Frame;
      end if;
      return To_Frame_Ptr (Malloc (Scope.Size));
   end Allocate_Frame;

   function Allocate_Frame (N : Node) return Frame_Ptr is
   begin
      return Allocate_Frame (Get_Scope (N));
   end Allocate_Frame;

   procedure Deallocate_Frame (Frame : in out Frame_Ptr) is
   begin
      if Frame /= null then
         Free (Frame.all'Address);
         Frame := null;
      end if;
   end Deallocate_Frame;

   function Allocate_Proc (Proc : Node) return Process_Acc
   is
      Res : Process_Acc;
   begin
      Res := new Process_Type'(Kind => Process_User,
                               Is_Scheduled => False,
                               Stmt => Proc,
                               Wakeup => 0,
                               Nref => 0,
                               Enabled_Chain => null,
                               Pause => 0,
                               Count => 0,
                               Link => (Origin => Proc,
                                        Frame => Global_Frame,
                                        Pc => Get_Statement (Proc)));

      return Res;
   end Allocate_Proc;

   procedure Add_Updates (Ref : Node; El : Update_El_Acc)
   is
      Typ : Node;
      Upd : Update_Acc;
   begin
      Upd := Get_Var_Update (Ref);

      if Upd = null then
         Typ := Get_Type_Data_Type (Ref);
         case Get_Kind (Typ) is
            when N_Logic_Type =>
               Upd := new Update_Type (Val_Logic);
            when N_Real_Type
              | N_Shortreal_Type =>
               Upd := new Update_Type (Val_Real);
            when N_Log_Packed_Array_Cst
              | N_Bit_Packed_Array_Cst =>
               Upd := new Update_Type (Val_Vector);
            when N_Array_Cst =>
               Upd := new Update_Type (Val_Array);
            when others =>
               Error_Kind ("add_updates(type)", Typ);
         end case;
         Set_Var_Update (Ref, Upd);
      end if;

      El.Next := Upd.List;
      Upd.List := El;
   end Add_Updates;

   function Create_Process_Upd (Proc : Process_Acc; Pc : Node)
                               return Update_El_Acc
   is
      pragma Unreferenced (Pc);
      Upd : Update_El_Acc;
   begin
      Upd := new Update_El'(Kind => Update_Process,
                            Next => null,
                            Proc => Proc);
      return Upd;
   end Create_Process_Upd;
   pragma Unreferenced (Create_Process_Upd);

   procedure Add_Updates_Obj (Obj : Node; Sens : Sensitivity_El)
   is
      Upd : Update_El_Acc;
   begin
      case Sens.Kind is
         when Update_Process =>
            Upd := new Update_El'(Kind => Update_Process,
                                  Next => null,
                                  Proc => Sens.Proc);
            Sens.Proc.Nref := Sens.Proc.Nref + 1;
         when Update_Edge =>
            Upd := new Update_El'(Kind => Update_Edge,
                                  Next => null,
                                  Edge => Sens.Edge);
         when Update_Vpi =>
            raise Internal_Error;
      end case;

      --  Add to variable.
      Add_Updates (Obj, Upd);
   end Add_Updates_Obj;

   procedure Add_Updates_Edge (Edge : Node; Sens : Sensitivity_El)
   is
      Expr : constant Node := Strip_Names_And_Ports (Get_Expression (Edge));
      Proc : Edge_Process_Acc;
      Upd : Update_El_Acc;
   begin
      case Get_Kind (Expr) is
         when Nkinds_Nets
            | N_Var
            | N_Modport_Item =>
            null;
         when others =>
            --  TODO
            --  a. indexed bit
            --  b. vector: same as bit on the LSB.
            --  c. others: detector on expression.
            raise Internal_Error;
      end case;

      --  TODO: maybe there is already an edge for this net/var.

      --  Create an edge detector.
      Proc := new Edge_Process_Type'(Kind => Get_Kind (Edge),
                                     Expr => Expr,
                                     Prev => V_X,
                                     Updates => null);

      --  Attach SENS to this detector.
      Upd := new Update_El'(Kind => Update_Process,
                            Next => null,
                            Proc => Sens.Proc);
      Sens.Proc.Nref := Sens.Proc.Nref + 1;
      Proc.Updates := Upd;

      Add_Updates_Obj (Expr, Sensitivity_El'(Kind => Update_Edge,
                                             Edge => Proc));
   end Add_Updates_Edge;

   procedure Add_Updates
     (Expr : Node; Sens : Sensitivity_El; Is_Lvalue : Boolean := False) is
   begin
      case Get_Kind (Expr) is
         when N_Conversion
            | N_Type_Cast
            | N_Size_Cast =>
            pragma Assert (not Is_Lvalue);
            Add_Updates (Get_Expression (Expr), Sens);
         when N_Number
           | N_Parameter
           | N_Localparam
           | N_String_Literal
           | N_Enum_Name =>
            pragma Assert (not Is_Lvalue);
            return;
         when N_Bit_Select
           | N_Indexed_Name =>
            Add_Updates (Get_Expression (Expr), Sens);
            if not Is_Lvalue then
               Add_Updates (Get_Name (Expr), Sens);
            end if;
         when N_Part_Select_Cst
           | N_Member_Name =>
            if not Is_Lvalue then
               Add_Updates (Get_Name (Expr), Sens);
            end if;
         when N_Plus_Part_Select_Cst
           | N_Minus_Part_Select_Cst =>
            Add_Updates (Get_Base_Expr (Expr), Sens);
            if not Is_Lvalue then
               Add_Updates (Get_Name (Expr), Sens);
            end if;
         when N_Concatenation =>
            declare
               El : Node;
            begin
               El := Get_Expressions (Expr);
               while El /= Null_Node loop
                  Add_Updates (Get_Expression (El), Sens, Is_Lvalue);
                  El := Get_Chain (El);
               end loop;
            end;
         when N_Binary_Op
           | N_Short_Circuit_Op =>
            pragma Assert (not Is_Lvalue);
            Add_Updates (Get_Left (Expr), Sens);
            Add_Updates (Get_Right (Expr), Sens);
         when N_Unary_Op =>
            pragma Assert (not Is_Lvalue);
            Add_Updates (Get_Expression (Expr), Sens);
         when N_Cond_Op =>
            pragma Assert (not Is_Lvalue);
            Add_Updates (Get_Condition (Expr), Sens);
            Add_Updates (Get_Cond_True (Expr), Sens);
            Add_Updates (Get_Cond_False (Expr), Sens);
         when N_Replication_Cst =>
            pragma Assert (not Is_Lvalue);
            declare
               Item : Node;
            begin
               Item := Get_Expressions (Expr);
               while Item /= Null_Node loop
                  Add_Updates (Get_Expression (Item), Sens);
                  Item := Get_Chain (Item);
               end loop;
            end;
         when Nkinds_Net_Port =>
            if not Is_Lvalue then
               Add_Updates_Obj (Get_Redeclaration (Expr), Sens);
            end if;
         when N_Name
           | N_Hierarchical
           | N_Interface_Item =>
            if not Is_Lvalue then
               Add_Updates (Get_Declaration (Expr), Sens);
            end if;
         when Nkinds_Nets
           | N_Var =>
            if not Is_Lvalue then
               Add_Updates_Obj (Expr, Sens);
            end if;
         when N_Posedge =>
            pragma Assert (not Is_Lvalue);
            Add_Updates_Edge (Expr, Sens);
         when N_Or =>
            pragma Assert (not Is_Lvalue);
            Add_Updates (Get_Left (Expr), Sens);
            Add_Updates (Get_Right (Expr), Sens);
         when N_Call =>
            pragma Assert (not Is_Lvalue);
            declare
               Arg : Node;
            begin
               Arg := Get_Arguments (Expr);
               while Arg /= Null_Node loop
                  Add_Updates (Get_Expression (Arg), Sens);
                  Arg := Get_Chain (Arg);
               end loop;
            end;
         when others =>
            Error_Kind ("add_updates", Expr);
      end case;
   end Add_Updates;

   procedure Add_Implicit_Updates (Stmt : Node; Sens : Sensitivity_El) is
   begin
      if Stmt = Null_Node then
         return;
      end if;

      case Get_Kind (Stmt) is
         when N_If =>
            Add_Updates (Get_Condition (Stmt), Sens);
            Add_Implicit_Updates (Get_True_Stmt (Stmt), Sens);
            Add_Implicit_Updates (Get_False_Stmt (Stmt), Sens);
         when N_Noblk_Assign =>
            Add_Updates (Get_Lvalue (Stmt), Sens, True);
            Add_Updates (Get_Expression (Stmt), Sens);
         when N_Blocking_Assign =>
            Add_Updates (Get_Lvalue (Stmt), Sens, True);
            Add_Updates (Get_Expression (Stmt), Sens);
         when N_Seq_Block =>
            declare
               El : Node;
            begin
               --  FIXME: declarations ?
               El := Get_Statements_Chain (Stmt);
               while El /= Null_Node loop
                  Add_Implicit_Updates (El, Sens);
                  El := Get_Chain (El);
               end loop;
            end;
         when Nkinds_Case =>
            Add_Updates (Get_Expression (Stmt), Sens);
            declare
               Item : Node;
            begin
               Item := Get_Case_Items (Stmt);
               while Item /= Null_Node loop
                  if Get_Kind (Item) = N_Case_Item then
                     Add_Updates (Get_Expression (Item), Sens);
                  end if;
                  Add_Implicit_Updates (Get_Statement (Item), Sens);
                  Item := Get_Chain (Item);
               end loop;
            end;
         when others =>
            Error_Kind ("add_implicit_updates", Stmt);
      end case;
   end Add_Implicit_Updates;

   procedure Allocate_Assign (Asgn : Node)
   is
      Proc : Process_Acc;
      Sens : Sensitivity_El (Update_Process);
   begin
      --  Create the process.
      Proc := new Process_Type'(Kind => Process_Assign,
                                Is_Scheduled => False,
                                Stmt => Asgn,
                                Wakeup => 0,
                                Nref => 0,
                                Enabled_Chain => null);
      Sens := (Kind => Update_Process,
               Proc => Proc,
               Pc => Asgn);
      Add_Updates (Get_Expression (Asgn), Sens);
      Activate_Process (Proc);
   end Allocate_Assign;

   procedure Allocate_Implicit_Assign (Decl : Node)
   is
      Proc : Process_Acc;
      Sens : Sensitivity_El (Update_Process);
   begin
      Proc := new Process_Type'(Kind => Process_Implicit_Assign,
                                Is_Scheduled => False,
                                Stmt => Decl,
                                Wakeup => 0,
                                Nref => 0,
                                Enabled_Chain => null);
      Sens := (Kind => Update_Process,
               Proc => Proc,
               Pc => Null_Node);
      Add_Updates (Get_Expression (Decl), Sens);
      Activate_Process (Proc);
   end Allocate_Implicit_Assign;

   procedure Allocate_Gate (Gate : Node)
   is
      Proc : Process_Acc;
      Sens : Sensitivity_El (Update_Process);
      Term : Node;
   begin
      Proc := new Process_Type'(Kind => Process_Gate,
                                Is_Scheduled => False,
                                Stmt => Gate,
                                Wakeup => 0,
                                Nref => 0,
                                Enabled_Chain => null);
      Sens := (Kind => Update_Process,
               Proc => Proc,
               Pc => Null_Node);
      Term := Get_Gate_Terminals (Gate);
      while Term /= Null_Node loop
         case Nkinds_Terminal (Get_Kind (Term)) is
            when N_Input_Terminal
              | N_Inout_Terminal
              | N_Control_Terminal =>
               Add_Updates (Get_Expression (Term), Sens);
            when N_Output_Terminal =>
               null;
         end case;
         Term := Get_Chain (Term);
      end loop;
      Activate_Process (Proc);
   end Allocate_Gate;

   procedure Allocate_Data (Scope : Scope_Acc; N : Node);

   procedure Allocate_Data_Chain (Scope : Scope_Acc; First : Node)
   is
      Item : Node;
   begin
      Item := First;
      while Item /= Null_Node loop
         Allocate_Data (Scope, Item);
         Item := Get_Chain (Item);
      end loop;
   end Allocate_Data_Chain;

   procedure Allocate_Class (N : Node)
   is
      Base : constant Node := Get_Type_Base_Class_Type (N);
      Cscope : Scope_Acc;
      Off : Storage_Index;
   begin
      pragma Assert (Get_Scope_Id (N) = No_Scope_Id);

      --  Avoid recursion.
      Set_Size_Flag (N, True);

      if Base /= Null_Node then
         --  Must be a class or an instantiated_class.
         Allocate_Type (Base);
      end if;

      Cscope := new Scope_Type'(Kind => Scope_Class,
                                First | Last => null,
                                Size => 0,
                                Align => 1);
      Scopes.Append (Cscope);
      Set_Scope_Id (N, Scopes.Last);

      if Base = Null_Node then
         --  Class link
         Allocate_Data_Raw (Cscope, Off, Class_Link_Size, Class_Link_Align);
         pragma Assert (Off = Class_Link_Offset);
      else
         declare
            Base_Scope : constant Scope_Acc := Get_Scope (Base);
         begin
            --  Inherit
            Cscope.Size := Base_Scope.Size;
            Cscope.Align := Base_Scope.Align;
         end;
      end if;

      Allocate_Data_Chain (Cscope, Get_Class_Item_Chain (N));
   end Allocate_Class;

   procedure Allocate_Type (N : Node) is
   begin
      if Get_Size_Flag (N) then
         return;
      end if;

      case Get_Kind (N) is
         when N_Logic_Type
            | N_Bit_Type =>
            Set_Size_Flag (N, True);
         when N_Log_Packed_Array_Cst
            | N_Bit_Packed_Array_Cst =>
            Set_Size_Flag (N, True);
         when N_Enum_Type
            | N_Packed_Struct_Type
            | N_Event_Type
            | N_Real_Type
            | N_Shortreal_Type
            | N_String_Type =>
            Set_Size_Flag (N, True);
         when N_Array_Cst =>
            declare
               El_Type : constant Node := Get_Type_Element_Type (N);
               El_Size : Storage_Index;
               Index_Length : Int32;
            begin
               Allocate_Type (El_Type);
               El_Size := Get_Storage_Size (El_Type);
               Set_Stride_Size (N, Tsize_Type (El_Size));
               Index_Length := Compute_Length (N);
               Set_Type_Size
                 (N, Tsize_Type (El_Size) * Tsize_Type (Index_Length));
               Set_Size_Flag (N, True);
            end;
         when N_Dynamic_Array_Cst =>
            declare
               El_Type : constant Node := Get_Type_Element_Type (N);
               El_Size : Storage_Index;
            begin
               Allocate_Type (El_Type);
               El_Size := Get_Storage_Size (El_Type);
               Set_Stride_Size (N, Tsize_Type (El_Size));
               Set_Size_Flag (N, True);
            end;
         when N_Struct_Type =>
            if Get_Scope_Id (N) /= No_Scope_Id then
               --  Already allocated.
               return;
            end if;
            declare
               Sscope : Scope_Acc;
            begin
               Sscope := new Scope_Type'(Kind => Scope_Struct,
                                         First | Last => null,
                                         Size => 0,
                                         Align => 1);
               Scopes.Append (Sscope);
               Set_Scope_Id (N, Scopes.Last);
               Allocate_Data_Chain (Sscope, Get_Members (N));
               --  Align the size of the structure.
               Align_Scope_Size (Sscope, Sscope.Align);
               Set_Type_Size (N, Tsize_Type (Sscope.Size));
            end;
         when N_Class =>
            pragma Assert (Get_Scope_Id (N) = No_Scope_Id);
            Allocate_Class (N);
         when N_Instantiated_Class =>
            pragma Assert (Get_Scope_Id (N) = No_Scope_Id);
            declare
               Param : Node;
               Param_Type : Node;
            begin
               Param := Get_Parameter_Port_Chain (N);
               while Param /= Null_Node loop
                  if Get_Kind (Param) = N_Type_Parameter then
                     Param_Type := Get_Parameter_Type (Param);
                     Allocate_Type (Param_Type);
                  end if;
                  Param := Get_Chain (Param);
               end loop;

               Allocate_Class (N);
            end;
         when N_Queue_Cst =>
            Allocate_Type (Get_Type_Element_Type (N));
            Set_Size_Flag (N, True);
         when N_Associative_Array_Cst =>
            declare
               Idx_Type : constant Node := Get_Type_Index_Type (N);
            begin
               if Idx_Type /= Null_Node then
                  Allocate_Type (Idx_Type);
               end if;
               Allocate_Type (Get_Type_Element_Type (N));
               Set_Size_Flag (N, True);
            end;
         when others =>
            Error_Kind ("allocate_type", N);
      end case;
   end Allocate_Type;

   procedure Allocate_Object_Type (N : Node)
   is
      Atype : constant Node := Get_Data_Type (N);
   begin
      Allocate_Type (Get_Expr_Type (Atype));
   end Allocate_Object_Type;

   procedure Allocate_Interface (N : Node)
   is
      Sscope : Scope_Acc;
   begin
      if Get_Scope_Id (N) /= No_Scope_Id then
         --  Already allocated.
         return;
      end if;

      Sscope := new Scope_Type'(Kind => Scope_Interface,
                                First | Last => null,
                                Size => 0,
                                Align => 1);
      Scopes.Append (Sscope);
      Set_Scope_Id (N, Scopes.Last);

      if Get_Parameter_Port_Chain (N) /= Null_Node then
         --  TODO.
         raise Internal_Error;
      end if;
      Allocate_Data_Chain (Sscope, Get_Ports_Chain (N));
      Allocate_Data_Chain (Sscope, Get_Items_Chain (N));

      --  Align the size of the structure.
      Align_Scope_Size (Sscope, Sscope.Align);
   end Allocate_Interface;

   procedure Allocate_Subroutine (N : Node)
   is
      Tf_Scope : Scope_Acc;
      Offset : Storage_Index;
      V : Node;
   begin
      pragma Assert (Get_Scope_Id (N) = No_Scope_Id);
      Tf_Scope := new Scope_Type'(Kind => Scope_Tf,
                                  First | Last => null,
                                  Size => 0,
                                  Align => 1);
      Scopes.Append (Tf_Scope);
      Set_Scope_Id (N, Scopes.Last);
      --  Allocate saved frame.
      Allocate_Data_Raw (Tf_Scope,
                         Offset,
                         Frame_Link_Size,
                         Frame_Link_Align);
      pragma Assert (Offset = Frame_Link_Offset);
      if Nkind_In (Get_Kind (N), N_Function, N_Extern_Function) then
         V := Get_Return_Variable (N);
         if V /= Null_Node then
            Allocate_Data (Tf_Scope, V);
         end if;
      end if;
      V := Get_This_Variable (N);
      if V /= Null_Node then
         Allocate_Data (Tf_Scope, V);
      end if;
      Allocate_Data_Chain (Tf_Scope, Get_Tf_Ports_Chain (N));
      Allocate_Data_Chain (Tf_Scope, Get_Tf_Item_Declaration_Chain (N));
      Allocate_Data_Chain (Tf_Scope, Get_Statements_Chain (N));
   end Allocate_Subroutine;

   procedure Allocate_Data (Scope : Scope_Acc; N : Node) is
   begin
      if N = Null_Node then
         --  For null statement.
         return;
      end if;

      case Get_Kind (N) is
         when N_Compilation_Unit =>
            Allocate_Data_Chain (Scope, Get_Descriptions (N));
         when N_Package =>
            Allocate_Data_Chain (Scope, Get_Package_Item_Chain (N));
         when N_Module
           | N_Program_Declaration =>
            null;
         when N_Module_Instance
           | N_Program_Instance =>
            declare
               Inst : constant Node := Get_Instance (N);
            begin
               Allocate_Collapsed_Connections (Scope, Get_Connections (N));
               Allocate_Data_Chain (Scope, Get_Parameter_Port_Chain (Inst));
               Allocate_Data_Chain (Scope, Get_Items_Chain (Inst));
            end;
         when Nkinds_Nets =>
            Allocate_Object_Type (N);
            Allocate_Var (Global_Scope, N, Get_Type_Data_Type (N));
         when N_Var
           | Nkinds_Tf_Port =>
            Allocate_Object_Type (N);
            Allocate_Var (Scope, N, Get_Type_Data_Type (N));
         when N_Return_Var
            | N_This_Var
            | N_Foreach_Variable =>
            --  Type is already allocated.
            Allocate_Var (Scope, N, Get_Expr_Type (N));
         when N_Class =>
            --  A class declaration
            --  Class may be allocated out of order (because of typedef).
            if Get_Scope_Id (N) = No_Scope_Id then
               Allocate_Class (N);
            end if;
         when N_Generic_Class =>
            null;
         when N_Interface_Declaration =>
            if Get_Parameter_Port_Chain (N) = Null_Node then
               Allocate_Interface (N);
            end if;
         when N_Modport =>
            null;
         when N_Interface_Instance =>
            Allocate_Interface_Instance (Global_Scope, N);
         when N_Typedef_Struct
           | N_Typedef_Class =>
            null;
         when N_Parameter
           | N_Localparam =>
            --  Already allocated.
            null;
         when Nkinds_Net_Port =>
            --  Allocated by the corresponding net/var.
            null;
         when Nkinds_Gate =>
            null;
         when N_Assign
           | N_Pack_Unpack_Assign =>
            null;
         when N_Initial
           | N_Always
           | N_Always_Comb =>
            --  No nested processes.
            Allocate_Data (Scope, Get_Statement (N));
         when N_Seq_Block
           | N_Par_Block =>
            --  TODO: allocate par_block ?
            Allocate_Data_Chain (Scope, Get_Block_Item_Declaration_Chain (N));
            Allocate_Data_Chain (Scope, Get_Statements_Chain (N));
         when N_Delay_Control =>
            Allocate_Data (Scope, Get_Statement (N));
         when N_Event_Control =>
            Allocate_Data (Scope, Get_Statement (N));
         when N_Blocking_Assign
           | N_Noblk_Assign
           | N_Assign_Operator
           | N_Pack_Assign
           | N_Unpack_Assign
           | N_Subroutine_Call_Stmt
           | N_Return_Stmt
           | N_Break_Stmt
           | N_Continue_Stmt
           | N_Pre_Increment
           | N_Post_Increment
           | N_Pre_Decrement
           | N_Post_Decrement
           | N_Trigger
           | N_Wait
           | N_Wait_Fork
           | N_Disable_Fork =>
            null;
         when N_Simple_Immediate_Assert =>
            Allocate_Data (Scope, Get_Pass_Stmt (N));
            Allocate_Data (Scope, Get_Else_Stmt (N));
         when N_If =>
            Allocate_Data (Scope, Get_True_Stmt (N));
            Allocate_Data (Scope, Get_False_Stmt (N));
         when N_For =>
            Allocate_Data (Scope, Get_For_Initialization (N));
            Allocate_Data (Scope, Get_Step_Assign (N));
            Allocate_Data (Scope, Get_Statement (N));
         when N_Repeat =>
            Allocate_Var (Scope, N, Get_Data_Type (N));
            Allocate_Data (Scope, Get_Statement (N));
         when N_While
           | N_Do_While
           | N_Forever =>
            Allocate_Data (Scope, Get_Statement (N));
         when N_Repeat_Control =>
            Allocate_Data (Scope, Get_Statement (N));
         when Nkinds_Case =>
            declare
               Item : Node;
               Stmt : Node;
            begin
               Item := Get_Case_Items (N);
               while Item /= Null_Node loop
                  Stmt := Get_Statement (Item);
                  Allocate_Data (Scope, Stmt);
                  Item := Get_Chain (Item);
               end loop;
            end;
         when N_Foreach =>
            Allocate_Data_Chain (Scope, Get_Foreach_Variables (N));
            Allocate_Data (Scope, Get_Statement (N));
         when N_Typedef =>
            Allocate_Type (Get_Type_Data_Type (N));
         when N_Function
           | N_Task =>
            Allocate_Subroutine (N);
         when N_Extern_Function
           | N_Extern_Task =>
            Allocate_Subroutine (N);
         when N_OOB_Function
           | N_OOB_Task =>
            null;
         when N_Member =>
            Allocate_Object_Type (N);
            Allocate_Member (Scope, N);
         when N_Genvar =>
            --  1800-2017 27.4 Loop generate constructs
            --  The genvar [...], but it does not exist at simulation time.
            null;
         when N_Generate_Region
           | N_Generate_Block
           | N_Array_Generate_Block
           | N_Indexed_Generate_Block =>
            Allocate_Data_Chain (Scope, Get_Generate_Item_Chain (N));
         when N_Loop_Generate
           | N_If_Generate =>
            --  Original code, that has been instantiated.
            null;
         when N_Package_Import =>
            null;
         when N_Assert_Property =>
            --  TODO
            null;
         when N_Import_DPI_Function
           | N_Export_DPI_Function =>
            --  TODO ?
            null;
         when N_Constraint =>
            --  TODO ?
            null;
         when others =>
            Error_Kind ("allocate_data", N);
      end case;
   end Allocate_Data;

   procedure Allocate_Procs
     (Scope : Scope_Acc; N : Node; Proc : Process_Acc);

   procedure Allocate_Procs_Chain
     (Scope : Scope_Acc; First : Node; Proc : Process_Acc)
   is
      Item : Node;
   begin
      Item := First;
      while Item /= Null_Node loop
         Allocate_Procs (Scope, Item, Proc);
         Item := Get_Chain (Item);
      end loop;
   end Allocate_Procs_Chain;

   procedure Allocate_Procs
     (Scope : Scope_Acc; N : Node; Proc : Process_Acc) is
   begin
      if N = Null_Node then
         --  For null statement.
         return;
      end if;

      case Get_Kind (N) is
         when N_Compilation_Unit =>
            pragma Assert (Proc = null);
            Allocate_Procs_Chain (Scope, Get_Descriptions (N), null);
         when N_Module =>
            pragma Assert (Proc = null);
            null;
         when N_Module_Instance
           | N_Program_Instance =>
            pragma Assert (Proc = null);
            declare
               Inst : constant Node := Get_Instance (N);
            begin
               Allocate_Connections_Process (Scope, Get_Connections (N));
               Allocate_Procs_Chain (Scope, Get_Items_Chain (Inst), null);
            end;
         when N_Interface_Instance =>
            pragma Assert (Proc = null);
            declare
               Decl : constant Node :=
                 Get_Declaration (Get_Interface_Name (N));
            begin
               Allocate_Connections_Process (Scope, Get_Connections (N));
               Allocate_Procs_Chain (Scope, Get_Items_Chain (Decl), null);
            end;
         when N_Var
            | N_Parameter
            | N_Localparam
            | Nkinds_Net_Port
            | N_Typedef
            | N_Typedef_Class
            | N_Typedef_Struct
            | N_Class
            | N_Modport =>
            null;
         when Nkinds_Nets =>
            if Get_Expression (N) /= Null_Node then
               pragma Assert (Proc = null);
               Allocate_Implicit_Assign (N);
            end if;
         when Nkinds_Gate =>
            pragma Assert (Proc = null);
            Allocate_Gate (N);
         when N_Assign =>
            pragma Assert (Proc = null);
            Allocate_Assign (N);
         when N_Initial
           | N_Always
           | N_Always_Comb =>
            --  No nested processes.
            pragma Assert (Proc = null);
            declare
               Proc1 : Process_Acc;
            begin
               Proc1 := Allocate_Proc (N);
               Allocate_Procs (Scope, Get_Statement (N), Proc1);
               Activate_Process (Proc1);
            end;
         when N_Seq_Block
           | N_Par_Block =>
            --  TODO: par_block: create threads ?
            Allocate_Procs_Chain
              (Scope, Get_Block_Item_Declaration_Chain (N), Proc);
            Allocate_Procs_Chain (Scope, Get_Statements_Chain (N), Proc);
         when N_Delay_Control =>
            Allocate_Procs (Scope, Get_Statement (N), Proc);
         when N_Event_Control =>
            declare
               Expr : constant Node := Get_Expression (N);
               Sens : Sensitivity_El (Update_Process);
            begin
               Sens := (Kind => Update_Process,
                        Proc => Proc,
                        Pc => N);
               if Get_Kind (Expr) = N_Implicit_Event then
                  Add_Implicit_Updates (Get_Statement (N), Sens);
               else
                  Add_Updates (Expr, Sens);
               end if;
               Allocate_Procs (Scope, Get_Statement (N), Proc);
            end;
         when N_Blocking_Assign
           | N_Noblk_Assign
           | N_Subroutine_Call_Stmt
           | N_Post_Increment =>
            null;
         when N_Repeat_Control =>
            Allocate_Procs (Scope, Get_Control (N), Proc);
            Allocate_Procs (Scope, Get_Statement (N), Proc);
         when N_Function
           | N_Task =>
            null;
         when N_Simple_Immediate_Assert =>
            Allocate_Procs (Scope, Get_Pass_Stmt (N), Proc);
            Allocate_Procs (Scope, Get_Else_Stmt (N), Proc);
         when N_If =>
            Allocate_Procs (Scope, Get_True_Stmt (N), Proc);
            Allocate_Procs (Scope, Get_False_Stmt (N), Proc);
         when N_For =>
            Allocate_Procs (Scope, Get_For_Initialization (N), Proc);
            Allocate_Procs (Scope, Get_Step_Assign (N), Proc);
            Allocate_Procs (Scope, Get_Statement (N), Proc);
         when N_While
           | N_Repeat
           | N_Forever =>
            Allocate_Procs (Scope, Get_Statement (N), Proc);
         when Nkinds_Case =>
            declare
               Item : Node;
            begin
               Item := Get_Case_Items (N);
               while Item /= Null_Node loop
                  Allocate_Procs (Scope, Get_Statement (Item), Proc);
                  Item := Get_Chain (Item);
               end loop;
            end;
         when N_Genvar =>
            null;
         when N_Loop_Generate
           | N_If_Generate =>
            null;
         when N_Generate_Region
           | N_Generate_Block
           | N_Array_Generate_Block
           | N_Indexed_Generate_Block =>
            pragma Assert (Proc = null);
            Allocate_Procs_Chain (Scope, Get_Generate_Item_Chain (N), null);
         when N_Package_Import =>
            null;
         when N_Assert_Property =>
            --  TODO.
            null;
         when others =>
            Error_Kind ("allocate_procs", N);
      end case;
   end Allocate_Procs;

   procedure Allocate_Resources (Units : Node; Root : Node) is
   begin
      pragma Assert (Global_Scope = null);
      Global_Scope := new Scope_Type'(Kind => Scope_Global,
                                      First => null,
                                      Last => null,
                                      Size => 0,
                                      Align => 1);
      Scopes.Append (Global_Scope);

      --  Data.
      Allocate_Data_Chain (Global_Scope, Units);
      Allocate_Data_Chain (Global_Scope, Get_Items_Chain (Root));

      --  Instantiated classes.
      declare
         use Verilog.Sem_Types;
         It : Instance_Class_Iterator;
         Cls : Node;
      begin
         Init_Instance_Class_Iterator (It);
         loop
            Cls := Get_Instance_Class_Iterator (It);
            exit when Cls = Null_Node;
            if Get_Scope_Id (Cls) = No_Scope_Id then
               Allocate_Class (Cls);
            end if;
            Next_Instance_Class_Iterator (It);
         end loop;
      end;

      Global_Frame := Allocate_Frame (Global_Scope);
      Init_Scope (Global_Frame, Global_Scope.First);

      --  Build processes.
      Allocate_Procs_Chain (Global_Scope, Get_Items_Chain (Root), null);
   end Allocate_Resources;

   type Node_Array is array (Natural range <>) of Node;
   No_Parents : constant Node_Array (0 .. -1) := (others => <>);

   procedure Disp_Decl (Parents : Node_Array; N : Node)
   is
      Parent : constant Node := Get_Parent (N);
   begin
      if Parent /= Null_Node then
         case Get_Kind (Parent) is
            when N_Module
              | N_For
              | N_If
              | N_Seq_Block
              | Nkinds_Process =>
               Disp_Decl (Parents, Parent);
               Put ('.');
            when N_Interface_Declaration =>
               Disp_Decl (Parents (0 .. Parents'Last - 1),
                          Parents (Parents'Last));
               Put ('.');
            when others =>
               Error_Kind ("disp_decl", Parent);
         end case;
      end if;

      case Get_Kind (N) is
         when N_For =>
            Put ("for");
         when N_Initial =>
            Put ("initial");
         when N_Always =>
            Put ("always");
         when N_Generate_Region =>
            Put ("*");
         when N_Event_Control
           | N_If =>
            Put ("?");
         when others =>
            Put (Name_Table.Image (Get_Identifier (N)));
      end case;
   end Disp_Decl;

   procedure Disp_Value (Value : Data_Ptr; Vtype : Node) is
   begin
      case Get_Kind (Vtype) is
         when N_Logic_Type =>
            Put (Log_To_Char (To_Logic_Ptr (Value).all));
         when N_Bit_Type =>
            Put (Bit_To_Char (To_Bit_Ptr (Value).all));
         when N_Shortreal_Type =>
            Put (Fp32'Image (To_Fp32_Ptr (Value).all));
         when N_Real_Type =>
            Put (Fp64'Image (To_Fp64_Ptr (Value).all));
         when N_Log_Packed_Array_Cst =>
            declare
               W : constant Width_Type := Get_Type_Width (Vtype);
               Last : constant Digit_Index := To_Last (W);
               Lv : constant Logvec_Ptr := To_Logvec_Ptr (Value);
               Hi : Natural;
            begin
               Hi := Natural ((W - 1) mod 32);
               for I in reverse 0 .. Last loop
                  declare
                     Val : constant Uns32 := Lv (I).Val;
                     Zx : constant Uns32 := Lv (I).Zx;
                     V : Uns32;
                  begin
                     for J in reverse 0 .. Hi loop
                        V := (Shift_Right (Val, J) and 1)
                          + (Shift_Right (Zx, J) and 1) * 2;
                        Put (Log_To_Char (Logic_Type'Val (V)));
                     end loop;
                     Hi := 31;
                  end;
               end loop;
            end;
         when N_Bit_Packed_Array_Cst =>
            declare
               W : constant Width_Type := Get_Type_Width (Vtype);
               Last : constant Digit_Index := To_Last (W);
               Bv : constant Bitvec_Ptr := To_Bitvec_Ptr (Value);
               Hi : Natural;
            begin
               Hi := Natural ((W - 1) mod 32);
               for I in reverse 0 .. Last loop
                  declare
                     Val : constant Uns32 := Bv (I);
                     V : Uns32;
                  begin
                     for J in reverse 0 .. Hi loop
                        V := Shift_Right (Val, J) and 1;
                        Put (Bit_To_Char (Bit_Type'Val (V)));
                     end loop;
                     Hi := 31;
                  end;
               end loop;
            end;
         when N_Array_Cst =>
            declare
               El_Type : constant Node := Get_Type_Element_Type (Vtype);
               Num : constant Int32 := Compute_Length (Vtype);
               Stride : constant Storage_Index :=
                 Storage_Index (Get_Stride_Size (Vtype));
               Off : Storage_Index;
            begin
               Put ("'{");
               Off := 0;
               for I in 1 .. Num loop
                  if I /= 1 then
                     Put (", ");
                  end if;
                  Disp_Value (Value + Off, El_Type);
                  Off := Off + Stride;
               end loop;
               Put ("}");
            end;
         when N_Struct_Type =>
            declare
               Member : Node;
            begin
               Member := Get_Members (Vtype);
               Put ("'{");
               loop
                  Put (Name_Table.Image (Get_Identifier (Member)));
                  Put (": ");
                  Disp_Value (Value + Get_Unpacked_Member_Offset (Member),
                              Get_Data_Type (Member));
                  Member := Get_Chain (Member);
                  exit when Member = Null_Node;
                  Put (", ");
               end loop;
               Put ("}");
            end;
         when N_String_Type =>
            declare
               Str : constant Sv_String_Ptr := To_Sv_String_Ptr (Value);
            begin
               if Str = null then
                  Put ("null");
               else
                  Put (Get_String (Str.all));
               end if;
            end;
         when N_Class
           | N_Instantiated_Class =>
            declare
               Hand : constant Sv_Class_Handle := To_Sv_Class_Ptr (Value).all;
            begin
               if Hand = null then
                  Put ("null");
               else
                  Put ("*handle*");
               end if;
            end;
         when N_Enum_Type =>
            Disp_Value (Value, Get_Enum_Base_Type (Vtype));
         when others =>
            Error_Kind ("disp_value", Vtype);
      end case;
   end Disp_Value;

   procedure Disp_Var (Frame : Frame_Ptr;
                       Parents : Node_Array;
                       Decl : Node;
                       With_Memories : Boolean)
   is
      Dtype : constant Node := Get_Type_Data_Type (Decl);
   begin
      Put ("[");
      Put_Trim (Obj_Id'Image (Get_Obj_Id (Decl)));
      Put ("] ");
      Disp_Decl (Parents, Decl);
      Put (": ");
      if With_Memories then
         Disp_Value (Get_Var_Data (Frame, Decl), Dtype);
      else
         case Get_Kind (Dtype) is
            when N_Array_Cst =>
               Put ("...");
            when others =>
               Disp_Value (Get_Var_Data (Frame, Decl), Dtype);
         end case;
      end if;
      New_Line;
   end Disp_Var;

   procedure Disp_Vars (Frame : Frame_Ptr;
                        Parents : Node_Array;
                        First : Obj_Acc;
                        With_Memories : Boolean)
   is
      Obj : Obj_Acc;
   begin
      Obj := First;
      while Obj /= null loop
         case Obj.Kind is
            when Obj_Frame_Var | Obj_Static_Var =>
               Disp_Var (Frame, Parents, Obj.Decl, With_Memories);
            when Obj_Interface_Instance =>
               Disp_Vars (Get_Sub_Frame (Frame, Obj),
                          Parents & Obj.Decl,
                          Obj.Decl_Scope.First, With_Memories);
            when Obj_Member =>
               raise Program_Error;
         end case;
         Obj := Obj.Next;
      end loop;
   end Disp_Vars;

   procedure Disp_All_Vars (With_Memories : Boolean) is
   begin
      Disp_Vars (Global_Frame, No_Parents, Global_Scope.First, With_Memories);
   end Disp_All_Vars;

   procedure Disp_Var_Update (Obj : Obj_Acc)
   is
      Ua : Update_Acc;
      Upd : Update_El_Acc;
      Proc : Process_Acc;
   begin
      Disp_Var (null, No_Parents, Obj.Decl, True);
      case Get_Kind (Obj.Decl) is
         when N_Parameter
           | N_Return_Var =>
            Ua := null;
         when N_Var =>
            if Get_Is_Automatic (Obj.Decl) then
               Ua := null;
            else
               Ua := Get_Var_Update (Obj.Decl);
            end if;
         when Nkinds_Nets =>
            Ua := Get_Var_Update (Obj.Decl);
         when others =>
            Error_Kind ("disp_var_update", Obj.Decl);
      end case;
      if Ua = null then
         Upd := null;
      else
         Upd := Ua.List;
      end if;
      while Upd /= null loop
         Put (" ");
         case Upd.Kind is
            when Update_Edge =>
               Put ("edge");
            when Update_Vpi =>
               Put ("vpi");
            when Update_Process =>
               Proc := Upd.Proc;
               case Proc.Kind is
                  when Process_User =>
                     Put ("user-process");
                  when Process_Assign =>
                     Put ("assign");
                  when Process_Implicit_Assign =>
                     Put ("net");
                  when Process_Gate =>
                     Put ("gate");
                  when Process_Conn_Input =>
                     Put ("conn_in");
                  when Process_Conn_Output =>
                     Put ("conn_out");
                  when Process_Conn_Default =>
                     Put ("conn_default");
               end case;
               Put (" at ");
               Put (Files_Map.Image (Get_Location (Proc.Stmt)));
         end case;
         New_Line;
         Upd := Upd.Next;
      end loop;
   end Disp_Var_Update;

   procedure Disp_All_Vars_Update
   is
      Obj : Obj_Acc;
   begin
      Obj := Global_Scope.First;
      while Obj /= null loop
         Disp_Var_Update (Obj);
         Obj := Obj.Next;
      end loop;
   end Disp_All_Vars_Update;

   --  For parameters and localparams
   type Param_Obj_Type is record
      Param : Node;
      Data : Data_Ptr;
   end record;

   package Params is new Tables
     (Table_Component_Type => Param_Obj_Type,
      Table_Index_Type => Obj_Id,
      Table_Low_Bound => No_Obj_Id + 1,
      Table_Initial => 32);

   procedure Allocate_Parameter (Param : Node; Expr : Node)
   is
      Param_Type : constant Node := Get_Param_Type (Param);
      Data : Data_Ptr;

      Ssize : Storage_Index;
   begin
      pragma Assert (Get_Obj_Id (Param) = No_Obj_Id);

      Allocate_Type (Param_Type);
      Ssize := Get_Storage_Size (Param_Type);
      Data := Malloc (Ssize);
      pragma Assert (not Is_Null (Data));

      Execute_Expression (null, Data, Expr);

      Params.Append ((Param => Param, Data => Data));
      Set_Obj_Id (Param, Params.Last);
   end Allocate_Parameter;

   function Get_Parameter_Data (Param : Node) return Data_Ptr
   is
      Id : constant Obj_Id := Get_Obj_Id (Param);
   begin
      return Params.Table (Id).Data;
   end Get_Parameter_Data;
end Verilog.Allocates;
