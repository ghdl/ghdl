--  Debugger for VHDL simulation
--  Copyright (C) 2022 Tristan Gingold
--
--  This file is part of GHDL.
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

with System;
with Ada.Unchecked_Conversion;

with GNAT.OS_Lib;

with Types; use Types;
with Name_Table; use Name_Table;
with Simple_IO; use Simple_IO;
with Utils_IO; use Utils_IO;
with Debuggers; use Debuggers;

with Vhdl.Utils; use Vhdl.Utils;
with Vhdl.Errors;

with Elab.Memtype;
with Elab.Vhdl_Objtypes; use Elab.Vhdl_Objtypes;
with Elab.Debugger; use Elab.Debugger;
with Elab.Vhdl_Debug; use Elab.Vhdl_Debug;
with Elab.Vhdl_Values; use Elab.Vhdl_Values;
with Elab.Vhdl_Values.Debug; use Elab.Vhdl_Values.Debug;
with Simul.Vhdl_Elab; use Simul.Vhdl_Elab;
with Simul.Vhdl_Simul;
with Simul.Main;

with Grt.Types; use Grt.Types;
with Grt.Vhdl_Types; use Grt.Vhdl_Types;
with Grt.Options;
with Grt.Processes;
with Grt.Signals; use Grt.Signals;
with Grt.Disp_Signals;
with Grt.Rtis_Addr;
with Grt.Stdio;

package body Simul.Vhdl_Debug is

   procedure Put_Time (Time : Std_Time)
   is
      use Grt.Options;
      Unit : Natural_Time_Scale;
      T : Std_Time;
   begin
      if Time = Std_Time'First then
         Put ("-Inf");
      else
         --  Do not bother with sec, min, and hr.
         Unit := Time_Resolution_Scale;
         T := Time;
         while Unit > 1 and then (T mod 1_000) = 0 loop
            T := T / 1000;
            Unit := Unit - 1;
         end loop;
         Put_Int64 (Int64 (T));
         case Unit is
            when 0 =>
               Put ("sec");
            when 1 =>
               Put ("ms");
            when 2 =>
               Put ("us");
            when 3 =>
               Put ("ns");
            when 4 =>
               Put ("ps");
            when 5 =>
               Put ("fs");
         end case;
      end if;
   end Put_Time;

   procedure Disp_Driver_Entry (D : Driver_Entry) is
   begin
      Put (" [");
      Put_Uns32 (Uns32 (D.Proc));
      Put ("] ");
      Disp_Instance_Path
        (Grt.Stdio.stdout, Processes_Table.Table (D.Proc).Inst);
      New_Line;
      Put ("    noff: ");
      Put_Uns32 (D.Sig.Offs.Net_Off);
      Put (", moff: ");
      Put_Uns32 (Uns32 (D.Sig.Offs.Mem_Off));
      Put (", len: ");
      Put_Uns32 (D.Sig.Typ.W);
      Put (", typ: ");
      Debug_Type_Short (D.Sig.Typ);
      New_Line;
   end Disp_Driver_Entry;

   procedure Disp_Conn_Endpoint (Ep : Sub_Signal_Type) is
   begin
      Put ("sig: ");
      Put_Uns32 (Uns32 (Ep.Base));
      Put (", noff: ");
      Put_Uns32 (Ep.Offs.Net_Off);
      Put (", moff: ");
      Put_Uns32 (Uns32 (Ep.Offs.Mem_Off));
      Put (", typ: ");
      Debug_Type_Short (Ep.Typ);
   end Disp_Conn_Endpoint;

   procedure Disp_Conn_Entry (Idx : Connect_Index_Type)
   is
      C : Connect_Entry renames Connect_Table.Table (Idx);
   begin
      Put ("    ");
      Put_Uns32 (Uns32 (Idx));
      Put (": ");
      if C.Collapsed then
         Put ("[collapsed]");
      end if;
      New_Line;
      Put ("     formal: ");
      Disp_Conn_Endpoint (C.Formal);
      New_Line;
      Put ("     actual: ");
      Disp_Conn_Endpoint (C.Actual);
      New_Line;
   end Disp_Conn_Entry;

   procedure Disp_Value (Value_Ptr : Ghdl_Value_Ptr;
                         Mode : Mode_Type;
                         Btype : Node)
   is
      use Grt.Stdio;
   begin
      case Mode is
         when Mode_B1 =>
            Disp_Enumeration_Value (stdout, Ghdl_B1'Pos (Value_Ptr.B1), Btype);
         when Mode_E8 =>
            Disp_Enumeration_Value (stdout, Int64 (Value_Ptr.E8), Btype);
         when Mode_E32 =>
            Disp_Enumeration_Value (stdout, Int64 (Value_Ptr.E32), Btype);
         when Mode_I32 =>
            Disp_Integer_Value (stdout, Int64 (Value_Ptr.I32), Btype);
         when Mode_I64 =>
            Disp_Integer_Value (stdout, Int64 (Value_Ptr.I64), Btype);
         when Mode_F64 =>
            Disp_Float_Value (stdout, Fp64 (Value_Ptr.F64), Btype);
      end case;
   end Disp_Value;

   procedure Disp_Value (Value : Value_Union;
                         Mode : Mode_Type;
                         Btype : Node)
   is
      use Grt.Stdio;
   begin
      case Mode is
         when Mode_B1 =>
            Disp_Enumeration_Value (stdout, Ghdl_B1'Pos (Value.B1), Btype);
         when Mode_E8 =>
            Disp_Enumeration_Value (stdout, Int64 (Value.E8), Btype);
         when Mode_E32 =>
            Disp_Enumeration_Value (stdout, Int64 (Value.E32), Btype);
         when Mode_I32 =>
            Disp_Integer_Value (stdout, Int64 (Value.I32), Btype);
         when Mode_I64 =>
            Disp_Integer_Value (stdout, Int64 (Value.I64), Btype);
         when Mode_F64 =>
            Disp_Float_Value (stdout, Fp64 (Value.F64), Btype);
      end case;
   end Disp_Value;

   procedure Disp_Transaction (Trans : Transaction_Acc;
                               Sig_Type : Node;
                               Mode : Mode_Type)
   is
      T : Transaction_Acc;
   begin
      T := Trans;
      loop
         case T.Kind is
            when Trans_Value =>
               Disp_Value (T.Val, Mode, Sig_Type);
            when Trans_Direct =>
               Disp_Value (T.Val_Ptr, Mode, Sig_Type);
            when Trans_Null =>
               Put ("NULL");
            when Trans_Error =>
               Put ("ERROR");
         end case;
         if T.Kind = Trans_Direct then
            --  The Time field is not updated for direct transaction.
            Put ("[DIRECT]");
         else
            Put ("@");
            Put_Time (T.Time);
         end if;
         T := T.Next;
         exit when T = null;
         Put (", ");
      end loop;
   end Disp_Transaction;

   procedure Info_Scalar_Signal_Driver (S : Memtyp; Stype : Node)
   is
      function To_Address is new Ada.Unchecked_Conversion
        (Source => Resolved_Signal_Acc, Target => System.Address);
      Sig : Ghdl_Signal_Ptr;
   begin
      Sig := Simul.Vhdl_Simul.Read_Sig (S.Mem);
      if Sig = null then
         Put_Line ("*not yet elaborated*");
         return;
      end if;
      Put_Addr (Sig.all'Address);
      Put (' ');
      Grt.Disp_Signals.Disp_Single_Signal_Attributes (Sig);
      Put (" val=");
      Disp_Value (Sig.Value_Ptr, Sig.Mode, Stype);
      Put ("; drv=");
      Disp_Value (Sig.Driving_Value, Sig.Mode, Stype);
      Put ("; last_val=");
      Disp_Value (Sig.Last_Value, Sig.Mode, Stype);
      if Sig.Nbr_Ports > 0 then
         Put (';');
         Put_Int32 (Int32 (Sig.Nbr_Ports));
         Put (" ports");
      end if;
      case Sig.S.Mode_Sig is
         when Mode_Signal_User =>
            if Sig.S.Resolv /= null then
               Put (" resolver=");
               Put_Addr (To_Address (Sig.S.Resolv));
            end if;
            if Sig.S.Nbr_Drivers = 0 then
               Put ("; no driver");
            elsif Sig.S.Nbr_Drivers = 1 then
               Put ("; trans=");
               Disp_Transaction
                 (Sig.S.Drivers (0).First_Trans, Stype, Sig.Mode);
            else
               for I in 0 .. Sig.S.Nbr_Drivers - 1 loop
                  New_Line;
                  Put ("   ");
--                  Disp_Context
--                    (Processes.Get_Rti_Context (Sig.S.Drivers (I).Proc));
                  Put (": ");
                  Disp_Transaction
                    (Sig.S.Drivers (I).First_Trans, Stype, Sig.Mode);
               end loop;
            end if;

         when Mode_Delayed =>
            Put ("; trans=");
            Disp_Transaction (Sig.S.Attr_Trans, Stype, Sig.Mode);

         when others =>
            null;
      end case;
      New_Line;
   end Info_Scalar_Signal_Driver;

   generic
      with procedure For_Scalar_Signal (S : Memtyp; Stype : Node);
   procedure For_Each_Scalar_Signal (S : Memtyp; Stype : Node);

   procedure For_Each_Scalar_Signal (S : Memtyp; Stype : Node) is
   begin
      case S.Typ.Kind is
         when Type_Scalars =>
            For_Scalar_Signal (S, Get_Base_Type (Stype));
         when Type_Vector
           | Type_Array =>
            declare
               use Simul.Vhdl_Simul;
               Len : constant Uns32 := S.Typ.Abound.Len;
               El_Type : Node;
               Stride : Uns32;
            begin
               if S.Typ.Alast then
                  El_Type := Get_Element_Subtype (Stype);
               else
                  El_Type := Stype;
               end if;
               Stride := S.Typ.Arr_El.W;

               for I in 1 .. Len loop
                  For_Each_Scalar_Signal
                    ((S.Typ.Arr_El, Sig_Index (S.Mem, (Len - I) * Stride)),
                     El_Type);
               end loop;
            end;
         when Type_Record =>
            declare
               use Simul.Vhdl_Simul;
               El_List : constant Iir_Flist :=
                 Get_Elements_Declaration_List (Stype);
               El : Node;
            begin
               for I in S.Typ.Rec.E'Range loop
                  El := Get_Nth_Element (El_List, Natural (I - 1));
                  Put (Image (Get_Identifier (El)));
                  Put (": ");
                  For_Each_Scalar_Signal
                    ((S.Typ.Rec.E (I).Typ,
                      Sig_Index (S.Mem, S.Typ.Rec.E (I).Offs.Net_Off)),
                     Get_Type (El));
               end loop;
            end;
         when Type_Unbounded_Vector
           | Type_Unbounded_Record
           | Type_Array_Unbounded
           | Type_Unbounded_Array
           | Type_Slice
           | Type_Protected
           | Type_File
           | Type_Access =>
            raise Internal_Error;
      end case;
   end For_Each_Scalar_Signal;

   procedure Info_Signal_Driver is new For_Each_Scalar_Signal
     (Info_Scalar_Signal_Driver);

   procedure Info_Scalar_Signal_Action (S : Memtyp; Stype : Node)
   is
      pragma Unreferenced (Stype);
      use Grt.Rtis_Addr;
      use Grt.Processes;
      Sig : Ghdl_Signal_Ptr;
      Ev : Action_List_Acc;
      Ctxt : Rti_Context;
   begin
      Sig := Simul.Vhdl_Simul.Read_Sig (S.Mem);
      if Sig = null then
         Put_Line ("*not yet elaborated*");
         return;
      end if;
      Put_Addr (Sig.all'Address);
      Put (':');
      Ev := Sig.Event_List;
      while Ev /= null loop
         Ctxt := Get_Rti_Context (Ev.Proc);
         Put (' ');
         Put_Addr (Ctxt.Base);
         Ev := Ev.Next;
      end loop;
      New_Line;
   end Info_Scalar_Signal_Action;

   procedure Info_Signal_Action is new For_Each_Scalar_Signal
     (Info_Scalar_Signal_Action);

   type Info_Signal_Options is record
      Max_Sz : Size_Type;
      Value : Boolean;
      Conn : Boolean;
      Types : Boolean;
      Sources : Boolean;
      Drivers : Boolean;
      Actions : Boolean;
   end record;

   procedure Put_Signal_Name (Inst : Synth_Instance_Acc; Decl : Node) is
   begin
      Disp_Instance_Path (Grt.Stdio.stdout, Inst, True);
      Put ('/');

      case Get_Kind (Decl) is
         when Iir_Kind_Signal_Declaration =>
            Put (Image (Get_Identifier (Decl)));
            Put (" [sig]");
         when Iir_Kind_Interface_Signal_Declaration =>
            Put (Image (Get_Identifier (Decl)));
            case Get_Mode (Decl) is
               when Iir_In_Mode =>
                  Put (" [in]");
               when Iir_Out_Mode =>
                  Put (" [out]");
               when Iir_Buffer_Mode =>
                  Put (" [buffer]");
               when Iir_Linkage_Mode =>
                  Put (" [linkage]");
               when Iir_Inout_Mode =>
                  Put (" [inout]");
               when Iir_Unknown_Mode =>
                  Put (" [??]");
            end case;
         when Iir_Kind_Guard_Signal_Declaration =>
            Put (Image (Get_Identifier (Decl)));
            Put (" [guard]");
         when Iir_Kind_Above_Attribute =>
            Put (" [above]");
         when Iir_Kind_Delayed_Attribute =>
            Put (" [delayed]");
         when others =>
            raise Internal_Error;
      end case;
   end Put_Signal_Name;

   procedure Info_Signal_Opts (Idx : Signal_Index_Type;
                               Opts : Info_Signal_Options)
   is
      use Elab.Memtype;
      S : Signal_Entry renames Signals_Table.Table (Idx);
      Nbr_Drv : Int32;
      Nbr_Sens : Int32;
      Sens : Sensitivity_Index_Type;
      Driver : Driver_Index_Type;
      Conn : Connect_Index_Type;
   begin
      Put_Int32 (Int32 (Idx));
      Put (": ");
      if S.Decl = Null_Iir then
         Put_Line ("??");
         return;
      end if;

      Put_Signal_Name (S.Inst, S.Decl);

      if Opts.Value = False then
         Put (" = ");
         if S.Typ.Sz > Opts.Max_Sz then
            Put ("[too large]");
         else
            Disp_Memtyp ((S.Typ, S.Val), Get_Type (S.Decl));
         end if;
      end if;
      New_Line;

      if Opts.Types then
         Put ("  type: ");
         Debug_Typ (S.Typ);
      end if;

      if Opts.Conn then
         if S.Kind = Signal_User then
            Nbr_Drv := 0;
            Driver := S.Drivers;
            while Driver /= No_Driver_Index loop
               Nbr_Drv := Nbr_Drv + 1;
               Driver := Drivers_Table.Table (Driver).Prev_Sig;
            end loop;
            Put ("  nbr drivers: ");
            Put_Int32 (Nbr_Drv);
            Put (", ");
         else
            Put ("  ");
         end if;

         Nbr_Sens := 0;
         Sens := S.Sensitivity;
         while Sens /= No_Sensitivity_Index loop
            Nbr_Sens := Nbr_Sens + 1;
            Sens := Sensitivity_Table.Table (Sens).Prev_Sig;
         end loop;

         Put ("nbr sensitivity: ");
         Put_Int32 (Nbr_Sens);

         Put (", collapsed_by: ");
         Put_Uns32 (Uns32 (S.Collapsed_By));
         New_Line;
      end if;

      if Opts.Sources and then S.Kind = Signal_User then
         Put ("  nbr sources (drv + conn : total):");
         New_Line;
         for I in 0 .. S.Typ.W - 1 loop
            Put ("    ");
            Put_Uns32 (I);
            Put (": ");
            Put_Uns32 (S.Nbr_Sources (I).Nbr_Drivers);
            Put (" + ");
            Put_Uns32 (S.Nbr_Sources (I).Nbr_Conns);
            Put (" : ");
            Put_Uns32 (S.Nbr_Sources (I).Total);
            New_Line;
         end loop;
      end if;

      if Opts.Conn then
         if S.Kind = Signal_User then
            Driver := S.Drivers;
            while Driver /= No_Driver_Index loop
               declare
                  D : Driver_Entry renames Drivers_Table.Table (Driver);
               begin
                  Put ("  driver:");
                  Disp_Driver_Entry (D);

                  Driver := D.Prev_Sig;
               end;
            end loop;

            Conn := S.Connect;
            if Conn /= No_Connect_Index then
               Put ("  connections:");
               New_Line;
               while Conn /= No_Connect_Index loop
                  declare
                     C : Connect_Entry renames Connect_Table.Table (Conn);
                  begin
                     Disp_Conn_Entry (Conn);
                     if C.Formal.Base = Idx then
                        Conn := C.Formal_Link;
                     else
                        pragma Assert (C.Actual.Base = Idx);
                        Conn := C.Actual_Link;
                     end if;
                  end;
               end loop;
            end if;
         end if;

         Sens := S.Sensitivity;
         while Sens /= No_Sensitivity_Index loop
            declare
               D : Driver_Entry renames Sensitivity_Table.Table (Sens);
            begin
               Put ("  sensitivity:");
               Disp_Driver_Entry (D);

               Sens := D.Prev_Sig;
            end;
         end loop;
      end if;

      if Opts.Value then
         Put ("value (");
         Put_Addr (S.Val.all'Address);
         Put ("): ");
         Disp_Memtyp ((S.Typ, S.Val), Get_Type (S.Decl));
         New_Line;
      end if;

      if Opts.Drivers and then S.Sig /= null then
         Put ("drivers (");
         Put_Addr (S.Sig.all'Address);
         Put ("): ");
         New_Line;
         Info_Signal_Driver ((S.Typ, S.Sig), Get_Type (S.Decl));
      end if;

      if Opts.Actions and then S.Sig /= null then
         Put_Line ("actions (for each scalar):");
         Info_Signal_Action ((S.Typ, S.Sig), Get_Type (S.Decl));
      end if;
   end Info_Signal_Opts;

   procedure Info_Signal (Idx : Signal_Index_Type) is
   begin
      Info_Signal_Opts (Idx, (Max_Sz => 256, others => True));
   end Info_Signal;

   --  For gdb.
   pragma Unreferenced (Info_Signal);

   function Find_Signal_By_Addr (Sig : Ghdl_Signal_Ptr)
                                return Signal_Index_Type
   is
      function Is_Signal_Inside (Addr : Ghdl_Signal_Ptr;
                                 S : Memtyp) return Boolean
      is
         use Simul.Vhdl_Simul;
      begin
         case S.Typ.Kind is
            when Type_Scalars =>
               return Read_Sig (S.Mem) = Addr;
            when Type_Vector
              | Type_Array =>
               declare
                  Len : constant Uns32 := S.Typ.Abound.Len;
                  Stride : constant Uns32 := S.Typ.Arr_El.W;
                  Sub : Memtyp;
               begin
                  Sub.Typ := S.Typ.Arr_El;
                  for I in 1 .. Len loop
                     Sub.Mem := Sig_Index (S.Mem, (Len - I) * Stride);
                     if Is_Signal_Inside (Addr, Sub) then
                        return True;
                     end if;
                  end loop;
               end;
            when Type_Record =>
               declare
                  Sub : Memtyp;
               begin
                  for I in S.Typ.Rec.E'Range loop
                     Sub.Mem := Sig_Index (S.Mem,
                                           S.Typ.Rec.E (I).Offs.Net_Off);
                     Sub.Typ := S.Typ.Rec.E (I).Typ;
                     if Is_Signal_Inside (Addr, Sub) then
                        return True;
                     end if;
                  end loop;
               end;
            when Type_Unbounded_Vector
              | Type_Unbounded_Record
              | Type_Array_Unbounded
              | Type_Unbounded_Array
              | Type_Slice
              | Type_Protected
              | Type_File
              | Type_Access =>
               raise Internal_Error;
         end case;
         return False;
      end Is_Signal_Inside;
   begin
      for I in Signals_Table.First .. Signals_Table.Last loop
         declare
            S : Signal_Entry renames Signals_Table.Table (I);
         begin
            if Is_Signal_Inside (Sig, (S.Typ, S.Sig)) then
               return I;
            end if;
         end;
      end loop;
      return No_Signal_Index;
   end Find_Signal_By_Addr;

   --  For gdb.
   pragma Unreferenced (Find_Signal_By_Addr);

   procedure Info_Signal_Proc (Line : String)
   is
      Opts : Info_Signal_Options;
      F, L : Natural;
      Idx : Uns32;
      Valid : Boolean;
   begin
      Opts := (Max_Sz => 80,
               others => False);
      Idx := 0;

      F := Line'First;
      loop
         F := Skip_Blanks (Line, F);
         exit when F > Line'Last;
         L := Get_Word (Line, F);
         if Line (F .. L) = "-h" then
            Put_Line ("info sig [OPTS] [SIG]");
            Put_Line (" -h   disp this help");
            Put_Line (" -v   disp values");
            Put_Line (" -d   disp drivers");
            Put_Line (" -a   disp actions");
            Put_Line (" -c   disp connections");
            Put_Line (" -t   disp types");
            Put_Line (" -s   disp sources");
            return;
         elsif Line (F .. L) = "-v" then
            Opts.Value := True;
         elsif Line (F .. L) = "-d" then
            Opts.Drivers := True;
         elsif Line (F .. L) = "-a" then
            Opts.Actions := True;
         elsif Line (F .. L) = "-c" then
            Opts.Conn := True;
         elsif Line (F .. L) = "-s" then
            Opts.Sources := True;
         elsif Line (F .. L) = "-t" then
            Opts.Types := True;
         elsif Line (F) in '0' .. '9' then
            To_Num (Line (F .. L), Idx, Valid);
            if not Valid
              or else Signal_Index_Type (Idx) > Signals_Table.Last
            then
               Put_Line ("invalid signal index");
               return;
            end if;
         else
            Put_Line ("unknown option");
            return;
         end if;
         F := L + 1;
      end loop;

      if Idx = 0 then
         for I in Signals_Table.First .. Signals_Table.Last loop
            Info_Signal_Opts (I, Opts);
         end loop;
      else
         Info_Signal_Opts (Signal_Index_Type (Idx), Opts);
      end if;
   end Info_Signal_Proc;

   procedure Disp_Quantity_Prefix (Decl : Node) is
   begin
      case Get_Kind (Decl) is
         when Iir_Kind_Free_Quantity_Declaration =>
            Put (Image (Get_Identifier (Decl)));
            Put (" (free)");
         when Iir_Kind_Across_Quantity_Declaration =>
            Put (Image (Get_Identifier (Decl)));
            Put (" (across)");
         when Iir_Kind_Through_Quantity_Declaration =>
            Put (Image (Get_Identifier (Decl)));
            Put (" (through)");
         when Iir_Kind_Dot_Attribute =>
            Disp_Quantity_Prefix (Get_Prefix (Decl));
            Put ("'dot");
         when Iir_Kinds_Denoting_Name =>
            Disp_Quantity_Prefix (Get_Named_Entity (Decl));
         when others =>
            Vhdl.Errors.Error_Kind ("disp_quantity_prefix", Decl);
      end case;
   end Disp_Quantity_Prefix;

   procedure Info_Scalar_Quantity (First : Scalar_Quantity_Index; Len : Uns32)
   is
      Idx : Scalar_Quantity_Index;
   begin
      if First = No_Scalar_Quantity then
         return;
      end if;
      Idx := First;
      for I in 1 .. Len loop
         declare
            use Simul.Vhdl_Simul;
            Sq : Scalar_Quantity_Record renames
              Scalar_Quantities_Table.Table (Idx);
         begin
            Put ("  scal #");
            Put_Uns32 (Uns32 (Idx));
            Put ("  y_idx: ");
            Put_Int32 (Int32 (Sq.Y_Idx));
            Put (", deriv: ");
            Put_Uns32 (Uns32 (Sq.Deriv));
            Put (", integ: ");
            Put_Uns32 (Uns32 (Sq.Integ));
            New_Line;
         end;
         Idx := Idx + 1;
      end loop;
   end Info_Scalar_Quantity;

   procedure Info_Quantity_Proc (Line : String)
   is
      pragma Unreferenced (Line);
   begin
      for I in Quantity_Table.First .. Quantity_Table.Last loop
         declare
            Q : Quantity_Entry renames Quantity_Table.Table (I);
         begin
            Put_Int32 (Int32 (I));
            Put (": ");

            Disp_Instance_Path (Grt.Stdio.stdout, Q.Inst, True);
            Put ('/');
            Disp_Quantity_Prefix (Q.Decl);
            Put ("  type: ");
            Debug_Type_Short (Q.Typ);
            Put (", len: ");
            Put_Uns32 (Q.Typ.W);
            Put (", Idx: ");
            Put_Uns32 (Uns32 (Q.Sq_Idx));
            Put (", val: ");
            Disp_Memtyp ((Q.Typ, Q.Val), Get_Type (Q.Decl));
            New_Line;
            Info_Scalar_Quantity (Q.Sq_Idx, Q.Typ.W);
         end;
      end loop;
   end Info_Quantity_Proc;

   procedure Info_Terminal_Proc (Line : String)
   is
      pragma Unreferenced (Line);
   begin
      for I in Terminal_Table.First .. Terminal_Table.Last loop
         declare
            T : Terminal_Entry renames Terminal_Table.Table (I);
         begin
            Put_Int32 (Int32 (I));
            Put (": ");

            Disp_Instance_Path (Grt.Stdio.stdout, T.Inst, True);
            Put ('/');
            Put (Image (Get_Identifier (T.Decl)));
            Put ("  across: ");
            Debug_Type_Short (T.Across_Typ);
            Put ("  through: ");
            Debug_Type_Short (T.Through_Typ);
            Put (", len: ");
            Put_Uns32 (T.Across_Typ.W);
            Put (", val: ");
            Disp_Memtyp ((T.Across_Typ, T.Ref_Val),
                         Get_Across_Type (Get_Nature (T.Decl)));
            New_Line;
            Info_Scalar_Quantity (T.Ref_Idx, T.Across_Typ.W);
         end;
      end loop;
   end Info_Terminal_Proc;

   procedure Info_Equations_Proc (Line : String)
   is
      pragma Unreferenced (Line);
   begin
      for I in Simultaneous_Table.First .. Simultaneous_Table.Last loop
         declare
            S : Simultaneous_Record renames Simultaneous_Table.Table (I);
         begin
            Put_Int32 (Int32 (I));
            Put (": ");

            Disp_Instance_Path (Grt.Stdio.stdout, S.Inst, True);
            New_Line;
         end;
      end loop;
   end Info_Equations_Proc;

   procedure Info_Time (Line : String)
   is
      pragma Unreferenced (Line);
   begin
      Put ("now: ");
      Put_Time (Current_Time);
      if Grt.Processes.Flag_AMS then
         Put ("  ");
         Put_Fp64 (Fp64 (Current_Time_AMS));
      end if;
      New_Line;
      Put ("delta: ");
      Put_Uns32 (Uns32 (Current_Delta));
      New_Line;
      Put ("next time: ");
      Put_Time (Grt.Processes.Next_Time);
      New_Line;
   end Info_Time;

   procedure Run_Proc (Line : String)
   is
      Delta_Time : Std_Time;
      P, L : Positive;
   begin
      P := Skip_Blanks (Line);
      if P <= Line'Last then
         L := Get_Word (Line, P);
         if Line (P .. L) = "-s" then
            Simul.Main.Break_Step := True;
         else
            Delta_Time := Grt.Options.Parse_Time (Line (P .. L));
            if Delta_Time = -1 then
               --  Error, ignore command.
               return;
            end if;
            Simul.Main.Break_Time := Current_Time + Delta_Time;
            Grt.Processes.Next_Time := Current_Time + Delta_Time;
         end if;
      end if;

      Elab.Debugger.Prepare_Continue;
   end Run_Proc;

   procedure Disp_Process (Idx : Process_Index_Type)
   is
      Proc : Proc_Record_Type renames Processes_Table.Table (Idx);
   begin
      Put_Uns32 (Uns32 (Idx));
      Put (": ");
      Disp_Instance_Path (Grt.Stdio.stdout, Proc.Inst);
      --  TODO: display label for non-process.
      Put ("  (");
      Put (Vhdl.Errors.Disp_Location (Proc.Proc));
      Put_Line (")");
   end Disp_Process;

   procedure Ps_Proc (Line : String)
   is
      P, L : Positive;
      Idx : Process_Index_Type;
      Val : Uns32;
      Valid : Boolean;
   begin
      Idx := No_Process_Index;

      P := Skip_Blanks (Line);
      if P <= Line'Last then
         L := Get_Word (Line, P);
         if Line (P .. L) = "-h" then
            Put_Line ("ps [PROC]");
            return;
         elsif Line (P) in '0' .. '9' then
            To_Num (Line (P .. L), Val, Valid);
            if not Valid or else Val > Uns32 (Processes_Table.Last) then
               Put_Line ("invalid process index");
               return;
            end if;
            Idx := Process_Index_Type (Val);
         else
            Put_Line ("unknown option");
            return;
         end if;
      end if;

      if Idx = No_Process_Index then
         for I in Processes_Table.First .. Processes_Table.Last loop
            Disp_Process (I);
         end loop;
      else
         Disp_Process (Idx);
      end if;
   end Ps_Proc;

   procedure Trace_Proc (Line : String)
   is
      Fn, Ln : Natural;
      Fv, Lv : Natural;
      State : Boolean;
   begin
      Fn := Skip_Blanks (Line, Line'First);
      if Fn > Line'Last then
         Put_Line ("missing trace name");
         return;
      end if;
      Ln := Get_Word (Line, Fn);

      Fv := Skip_Blanks (Line, Ln + 1);
      if Fv > Line'Last then
         Put_Line ("missing on/off/0/1");
         return;
      end if;
      Lv := Get_Word (Line, Fv);
      if Line (Fv .. Lv) = "on" or Line (Fv .. Lv) = "1" then
         State := True;
      elsif Line (Fv .. Lv) = "off" or Line (Fv .. Lv) = "0" then
         State := False;
      else
         Put_Line ("expect on/off/0/1");
         return;
      end if;

      if Line (Fn .. Ln) = "residues" then
         Simul.Vhdl_Simul.Trace_Residues := State;
      else
         Put_Line ("usage: trace residues on|off|0|1");
      end if;
   end Trace_Proc;

   procedure Quit_Proc (Line : String)
   is
      pragma Unreferenced (Line);
   begin
      GNAT.OS_Lib.OS_Exit (0);
   end Quit_Proc;

   procedure Init is
   begin
      Elab.Vhdl_Debug.Append_Commands;
      Append_Info_Command
        (new String'("sig*nal"),
         new String'("display info about a signal"),
         Info_Signal_Proc'Access);
      Append_Info_Command
        (new String'("quan*tity"),
         new String'("display info about quantities"),
         Info_Quantity_Proc'Access);
      Append_Info_Command
        (new String'("term*inal"),
         new String'("display info about terminals"),
         Info_Terminal_Proc'Access);
      Append_Info_Command
        (new String'("equ*ations"),
         new String'("display info about equations"),
         Info_Equations_Proc'Access);
      Append_Info_Command
        (new String'("t*ime"),
         new String'("display current time"),
         Info_Time'Access);
      Append_Menu_Command
        (new String'("r*un"),
         new String'("resume execution for an amount of time"),
         Run_Proc'Access);
      Append_Menu_Command
        (new String'("ps"),
         new String'("print all processes"),
         Ps_Proc'Access);
      Append_Menu_Command
        (new String'("trace"),
         new String'("enable/disable a trace"),
         Trace_Proc'Access);
      Append_Menu_Command
        (new String'("q*uit"),
         new String'("exit simulation"),
         Quit_Proc'Access);
   end Init;
end Simul.Vhdl_Debug;
