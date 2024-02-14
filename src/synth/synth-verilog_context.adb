--  Verilog data context for synthesis
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
--  along with GHDL; see the file COPYING.  If not, see:
--  <http://www.gnu.org/licenses>.

with System;
with Ada.Unchecked_Deallocation;

with Types; use Types;

with Verilog.Nutils; use Verilog.Nutils;
with Verilog.Allocates;

package body Synth.Verilog_Context is
   function Make_Root_Instance (Base : Base_Instance_Acc)
                               return Synth_Instance_Acc
   is
      Pool : Areapool_Acc;
      Marker : Mark_Type;
   begin
      --  Create a memory pool for temporary values.
      Pool := new Areapool;
      Mark (Marker, Pool.all);

      return new Synth_Instance_Type'(Base => Base,
                                      Name => No_Sname,
                                      Scope => null,
                                      Expr_Pool => Pool,
                                      Mark => Marker);
   end Make_Root_Instance;

   function Make_Sub_Instance (Parent : Synth_Instance_Acc;
                               Scope : Scope_Acc) return Synth_Instance_Acc
   is
      Marker : Mark_Type;
   begin
      Mark (Marker, Parent.Expr_Pool.all);
      return new Synth_Instance_Type'(Base => Parent.Base,
                                      Name => No_Sname,
                                      Scope => Scope,
                                      Expr_Pool => Parent.Expr_Pool,
                                      Mark => Marker);
   end Make_Sub_Instance;

   procedure Free_Instance (Synth_Inst : in out Synth_Instance_Acc) is
   begin
      null;
   end Free_Instance;

   function Get_Sname (Inst : Synth_Instance_Acc) return Sname is
   begin
      return Inst.Name;
   end Get_Sname;

   procedure Push_Sname
     (Inst : Synth_Instance_Acc; Name : Sname; Prev : out Sname) is
   begin
      Prev := Inst.Name;
      Inst.Name := Name;
   end Push_Sname;

   procedure Pop_Sname
     (Inst : Synth_Instance_Acc; Prev : Sname) is
   begin
      Inst.Name := Prev;
   end Pop_Sname;

   function Get_Scope (Inst : Synth_Instance_Acc) return Scope_Acc is
   begin
      return Inst.Scope;
   end Get_Scope;

   function Get_Build (Inst : Synth_Instance_Acc) return Context_Acc is
   begin
      return Inst.Base.Builder;
   end Get_Build;

   procedure Set_Module (Inst : Synth_Instance_Acc; M : Module) is
   begin
      Inst.Base.Cur_Module := M;
   end Set_Module;

   procedure Set_Current_Module (Inst : Synth_Instance_Acc; M : Module) is
   begin
      Inst.Base.Cur_Module := M;
      Set_Parent (Inst.Base.Builder, M);
   end Set_Current_Module;

   function Get_Module (Inst : Synth_Instance_Acc) return Module is
   begin
      return Inst.Base.Cur_Module;
   end Get_Module;

   function Get_Top_Module (Inst : Synth_Instance_Acc) return Module is
   begin
      return Inst.Base.Top_Module;
   end Get_Top_Module;

   function Get_Self_Instance (Inst : Synth_Instance_Acc) return Instance is
   begin
      return Get_Self_Instance (Get_Module (Inst));
   end Get_Self_Instance;

   function Allocate_Frame (Scope : Scope_Acc) return Frame_Ptr
   is
      function Malloc (S : Storage_Index) return Frame_Ptr;
      pragma Import (C, Malloc);
   begin
      if Scope.Size = 0 then
         return Null_Frame;
      end if;
      return Malloc (Scope.Size);
   end Allocate_Frame;

   procedure Allocate_Frame_For_Scope (Scope : Scope_Acc)
   is
      procedure Memcpy
        (Dest : Frame_Ptr; Src : Frame_Ptr; Len : Storage_Index);
      pragma Import (C, Memcpy);

      procedure Free (Ptr : Frame_Ptr);
      pragma Import (C, Free);

      procedure Deallocate is new
        Ada.Unchecked_Deallocation (Frame_Type, Frame_Acc);

      Frame : Frame_Acc;
   begin
      Frame := new Frame_Type (Last_Obj => Scope.Last_Obj,
                               Size => Scope.Size);
      Frame.Data := Allocate_Frame (Scope);
      --  TODO: copy old frame and free it.
      if Scope.Frame /= null then
         Frame.Objs (Scope.Frame.Objs'Range) := Scope.Frame.Objs;
         Memcpy (Frame.Data, Scope.Frame.Data, Scope.Frame.Size);
         Free (Scope.Frame.Data);
         Deallocate (Scope.Frame);
      end if;
      Scope.Frame := Frame;
   end Allocate_Frame_For_Scope;

   procedure Set_Obj (Scope : Scope_Acc; N : Node; Obj : Obj_Type)
   is
      Id : constant Obj_Id := Get_Obj_Id (N);
   begin
      pragma Assert (Scope.Frame /= null);
      case Scope.Frame.Objs (Id).Kind is
         when Obj_None
            | Obj_Port =>
            null;
         when others =>
            raise Internal_Error;
      end case;
      Scope.Frame.Objs (Id) := Obj;
   end Set_Obj;

   procedure Set_Obj_Wire (Inst : Synth_Instance_Acc; N : Node; Wid : Wire_Id)
   is
      Scope : constant Scope_Acc := Inst.Scope;
   begin
      Set_Obj (Scope, N, Obj_Type'(Kind => Obj_Wire,
                                   Decl => N,
                                   Wire => Wid));
   end Set_Obj_Wire;

   procedure Set_Obj_Value (Inst : Synth_Instance_Acc; N : Node; Val : Valtyp)
   is
   begin
      case Val.Kind is
         when Value_Memory =>
            Set_Obj (Inst.Scope, N, Obj_Type'(Kind => Obj_Memory,
                                              Decl => N,
                                              Mem => Val.Mem));
         when others =>
            raise Internal_Error;
      end case;
   end Set_Obj_Value;

   function Get_Obj_Value (Scope : Scope_Acc; N : Node) return Valtyp
   is
      Id : constant Obj_Id := Get_Obj_Id (N);
      Obj : Obj_Type renames Scope.Frame.Objs (Id);
      Typ : constant Node := Get_Type_Data_Type (N);
   begin
      case Obj.Kind is
         when Obj_Wire =>
            return (Kind => Value_Wire, Typ => Typ, W => Obj.Wire);
         when Obj_Net =>
            return (Kind => Value_Net, Typ => Typ, N => Obj.N);
         when Obj_Memory =>
            return (Kind => Value_Memory, Typ => Typ, Mem => Obj.Mem);
         when others =>
            raise Internal_Error;
      end case;
   end Get_Obj_Value;

   function Get_Obj_Value (Inst : Synth_Instance_Acc; N : Node) return Valtyp
   is
   begin
      return Get_Obj_Value (Inst.Scope, N);
   end Get_Obj_Value;

   procedure Set_Obj_Port (Scope : Scope_Acc; N : Node; Port : Port_Idx) is
   begin
      Set_Obj (Scope, N, Obj_Type'(Kind => Obj_Port,
                                   Decl => N,
                                   Port => Port));
   end Set_Obj_Port;

   function Get_Obj_Port (Inst : Synth_Instance_Acc; N : Node) return Port_Idx
   is
      Scope : constant Scope_Acc := Inst.Scope;
      Id : constant Obj_Id := Get_Obj_Id (N);
      Obj : Obj_Type renames Scope.Frame.Objs (Id);
   begin
      return Obj.Port;
   end Get_Obj_Port;

   procedure Set_Obj_Net (Inst : Synth_Instance_Acc;
                          Obj : Node; N : Net)
   is
      Scope : constant Scope_Acc := Inst.Scope;
   begin
      Set_Obj (Scope, Obj, Obj_Type'(Kind => Obj_Net,
                                     Decl => Obj,
                                     N => N));
   end Set_Obj_Net;

   function Allocate_Memory (Inst : Synth_Instance_Acc; Typ : Node)
                            return Memory_Ptr
   is
      use Verilog.Allocates;
      use System;
      Size : constant Storage_Index := Get_Storage_Size (Typ);
      Align : constant Storage_Index := Get_Storage_Align (Typ);
      Res : Address;
   begin
      Areapools.Allocate (Inst.Expr_Pool.all, Res,
                          Size_Type (Size), Size_Type (Align));
      return To_Memory_Ptr (Res);
   end Allocate_Memory;
end Synth.Verilog_Context;
