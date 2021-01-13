--  GHDL Run Time (GRT) - Display subprograms for signals.
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
--
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.
with System; use System;
with System.Storage_Elements; --  Work around GNAT bug.
pragma Unreferenced (System.Storage_Elements);
with Ada.Unchecked_Conversion;
with Grt.Rtis; use Grt.Rtis;
with Grt.Rtis_Addr; use Grt.Rtis_Addr;
with Grt.Rtis_Utils; use Grt.Rtis_Utils;
with Grt.Astdio; use Grt.Astdio;
with Grt.Astdio.Vhdl; use Grt.Astdio.Vhdl;
with Grt.Errors; use Grt.Errors;
pragma Elaborate_All (Grt.Rtis_Utils);
with Grt.Vstrings; use Grt.Vstrings;
with Grt.Vstrings_IO; use Grt.Vstrings_IO;
with Grt.Options;
with Grt.Processes;
with Grt.Disp; use Grt.Disp;

package body Grt.Disp_Signals is
   procedure Foreach_Scalar_Signal
     (Process : access procedure (Val_Addr : Address;
                                  Val_Name : Vstring;
                                  Val_Type : Ghdl_Rti_Access;
                                  Param : Rti_Object))
   is
      procedure Call_Process (Val_Addr : Address;
                              Val_Name : Vstring;
                              Val_Type : Ghdl_Rti_Access;
                              Param : Rti_Object) is
      begin
         Process.all (Val_Addr, Val_Name, Val_Type, Param);
      end Call_Process;

      pragma Inline (Call_Process);

      procedure Foreach_Scalar_Signal_Signal is new
        Foreach_Scalar (Param_Type => Rti_Object,
                        Process => Call_Process);

      function Foreach_Scalar_Signal_Object
        (Ctxt : Rti_Context; Obj : Ghdl_Rti_Access)
        return Traverse_Result
      is
         Sig : Ghdl_Rtin_Object_Acc;
      begin
         case Obj.Kind is
            when Ghdl_Rtik_Signal
              | Ghdl_Rtik_Port
              | Ghdl_Rtik_Guard
              | Ghdl_Rtik_Attribute_Quiet
              | Ghdl_Rtik_Attribute_Stable
              | Ghdl_Rtik_Attribute_Transaction =>
               Sig := To_Ghdl_Rtin_Object_Acc (Obj);
               Foreach_Scalar_Signal_Signal
                 (Ctxt, Sig.Obj_Type,
                  Loc_To_Addr (Sig.Common.Depth, Sig.Loc, Ctxt), True,
                  Rti_Object'(Obj, Ctxt));
            when others =>
               null;
         end case;
         return Traverse_Ok;
      end Foreach_Scalar_Signal_Object;

      function Foreach_Scalar_Signal_Traverse is
         new Traverse_Blocks (Process => Foreach_Scalar_Signal_Object);

      Res : Traverse_Result;
      pragma Unreferenced (Res);
   begin
      Res := Foreach_Scalar_Signal_Traverse (Get_Top_Context);
   end Foreach_Scalar_Signal;

   procedure Disp_Context (Ctxt : Rti_Context)
   is
      Blk : Ghdl_Rtin_Block_Acc;
      Nctxt : Rti_Context;
   begin
      --  Avoid fault.
      if Ctxt = Null_Context then
         Put ('?');
         return;
      end if;

      Blk := To_Ghdl_Rtin_Block_Acc (Ctxt.Block);
      case Blk.Common.Kind is
         when Ghdl_Rtik_Block
           | Ghdl_Rtik_Process =>
            Nctxt := Get_Parent_Context (Ctxt);
            Disp_Context (Nctxt);
            Put ('.');
            Put (Blk.Name);
         when Ghdl_Rtik_Entity =>
            Put (Blk.Name);
         when Ghdl_Rtik_Architecture =>
            Nctxt := Get_Parent_Context (Ctxt);
            Disp_Context (Nctxt);
            Put ('(');
            Put (Blk.Name);
            Put (')');
         when others =>
            Internal_Error ("disp_context");
      end case;
   end Disp_Context;

   --  Option --trace-signals.

   --  Disp transaction TRANS from signal SIG.
   procedure Disp_Transaction (Trans : Transaction_Acc;
                               Sig_Type : Ghdl_Rti_Access;
                               Mode : Mode_Type)
   is
      T : Transaction_Acc;
   begin
      T := Trans;
      loop
         case T.Kind is
            when Trans_Value =>
               if Sig_Type /= null then
                  Disp_Value (stdout, T.Val, Sig_Type);
               else
                  Disp_Value (T.Val, Mode);
               end if;
            when Trans_Direct =>
               if Sig_Type /= null then
                  Disp_Value (stdout, T.Val_Ptr.all, Sig_Type);
               else
                  Disp_Value (T.Val_Ptr.all, Mode);
               end if;
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
            Put_Time (stdout, T.Time);
         end if;
         T := T.Next;
         exit when T = null;
         Put (", ");
      end loop;
   end Disp_Transaction;

   procedure Disp_Flag (Name : Character; Cond : Boolean)
   is
      C : Character;
   begin
      if Cond then
         C := Name;
      else
         C := '-';
      end if;
      Put (C);
   end Disp_Flag;

   procedure Disp_Single_Signal_Attributes (Sig : Ghdl_Signal_Ptr) is
   begin
      Disp_Mode (Sig.Mode);
      Put (' ');
      Disp_Flag ('A', Sig.Active);
      Disp_Flag ('E', Sig.Event);
      Disp_Flag ('a', Sig.Has_Active);
      Disp_Flag ('e', Sig.S.Effective /= null);
      Disp_Flag ('F', Sig.Flags.Is_Drv_Forced);
      Disp_Flag ('f', Sig.Flags.Is_Eff_Forced);
      if Boolean'(True) then
         Put (" last_event=");
         Put_Time (stdout, Sig.Last_Event);
         Put (" last_active=");
         Put_Time (stdout, Sig.Last_Active);
      end if;
   end Disp_Single_Signal_Attributes;

   procedure Disp_Simple_Signal
     (Sig : Ghdl_Signal_Ptr; Sig_Type : Ghdl_Rti_Access; Sources : Boolean)
   is
      function To_Address is new Ada.Unchecked_Conversion
        (Source => Resolved_Signal_Acc, Target => Address);
   begin
      Put (' ');
      Put (stdout, Sig.all'Address);
      Put (' ');
      Disp_Single_Signal_Attributes (Sig);
      Put (" val=");
      declare
         Val : constant Value_Union := Read_Value (Sig.Value_Ptr, Sig.Mode);
      begin
         if Sig_Type /= null then
            Disp_Value (stdout, Val, Sig_Type);
         else
            Disp_Value (Val, Sig.Mode);
         end if;
      end;
      Put ("; drv=");
      if Sig_Type /= null then
         Disp_Value (stdout, Sig.Driving_Value, Sig_Type);
      else
         Disp_Value (Sig.Driving_Value, Sig.Mode);
      end if;
      if Boolean'(False) then
         Put ("; lst=");
         if Sig_Type /= null then
            Disp_Value (stdout, Sig.Last_Value, Sig_Type);
         else
            Disp_Value (Sig.Last_Value, Sig.Mode);
         end if;
      end if;
      if Sources then
         if Sig.Nbr_Ports > 0 then
            Put (';');
            Put_I32 (stdout, Ghdl_I32 (Sig.Nbr_Ports));
            Put (" ports");
         end if;
         case Sig.S.Mode_Sig is
            when Mode_Signal_User =>
               if Sig.S.Resolv /= null then
                  Put (stdout, " resolver=");
                  Put (stdout, To_Address(Sig.S.Resolv));
               end if;
               if Sig.S.Nbr_Drivers = 0 then
                  Put ("; no driver");
               elsif Sig.S.Nbr_Drivers = 1 then
                  Put ("; trans=");
                  Disp_Transaction
                    (Sig.S.Drivers (0).First_Trans, Sig_Type, Sig.Mode);
               else
                  for I in 0 .. Sig.S.Nbr_Drivers - 1 loop
                     New_Line;
                     Put ("   ");
                     Disp_Context
                       (Processes.Get_Rti_Context (Sig.S.Drivers (I).Proc));
                     Put (": ");
                     Disp_Transaction
                       (Sig.S.Drivers (I).First_Trans, Sig_Type, Sig.Mode);
                  end loop;
               end if;

            when Mode_Delayed =>
               Put ("; trans=");
               Disp_Transaction (Sig.S.Attr_Trans, Sig_Type, Sig.Mode);

            when others =>
               null;
         end case;
      end if;
      New_Line;
   end Disp_Simple_Signal;

   procedure Disp_Signal_Name (Stream : FILEs;
                               Ctxt : Rti_Context;
                               Sig : Ghdl_Rtin_Object_Acc)
   is
      procedure Disp_Prefix (Stream : FILEs; Ctxt : Rti_Context) is
      begin
         Put (Stream, Ctxt);
         Put (Stream, ".");
      end Disp_Prefix;
   begin
      case Sig.Common.Kind is
         when Ghdl_Rtik_Signal
           | Ghdl_Rtik_Port
           | Ghdl_Rtik_Guard =>
            Disp_Prefix (Stream, Ctxt);
            Put (Stream, Sig.Name);
         when Ghdl_Rtik_Attribute_Quiet =>
            Disp_Prefix (Stream, Ctxt);
            Put (Stream, " 'quiet");
         when Ghdl_Rtik_Attribute_Stable =>
            Disp_Prefix (Stream, Ctxt);
            Put (Stream, " 'stable");
         when Ghdl_Rtik_Attribute_Transaction =>
            Disp_Prefix (Stream, Ctxt);
            Put (Stream, " 'transaction");
         when others =>
            null;
      end case;
   end Disp_Signal_Name;

   procedure Disp_Scalar_Signal (Val_Addr : Address;
                                 Val_Name : Vstring;
                                 Val_Type : Ghdl_Rti_Access;
                                 Parent : Rti_Object) is
   begin
      Disp_Signal_Name (stdout, Parent.Ctxt,
                        To_Ghdl_Rtin_Object_Acc (Parent.Obj));
      Put (stdout, Val_Name);
      Disp_Simple_Signal (To_Ghdl_Signal_Ptr (To_Addr_Acc (Val_Addr).all),
                          Val_Type, Options.Disp_Sources);
   end Disp_Scalar_Signal;


   procedure Disp_All_Signals is
   begin
      Foreach_Scalar_Signal (Disp_Scalar_Signal'access);
   end Disp_All_Signals;

   --  Option disp-sensitivity

   procedure Disp_Scalar_Sensitivity (Val_Addr : Address;
                                      Val_Name : Vstring;
                                      Val_Type : Ghdl_Rti_Access;
                                      Parent : Rti_Object)
   is
      pragma Unreferenced (Val_Type);
      Sig : Ghdl_Signal_Ptr;

      Action : Action_List_Acc;
   begin
      Sig := To_Ghdl_Signal_Ptr (To_Addr_Acc (Val_Addr).all);
      if Sig.Flags.Seen then
         return;
      else
         Sig.Flags.Seen := True;
      end if;
      Disp_Signal_Name (stdout, Parent.Ctxt,
                        To_Ghdl_Rtin_Object_Acc (Parent.Obj));
      Put (stdout, Val_Name);
      New_Line (stdout);

      Action := Sig.Event_List;
      while Action /= null loop
         Put (stdout, "  wakeup ");
         Put (stdout, Grt.Processes.Get_Rti_Context (Action.Proc));
         New_Line (stdout);
         Action := Action.Next;
      end loop;

      if Sig.S.Mode_Sig in Mode_Signal_User then
         for I in 1 .. Sig.S.Nbr_Drivers loop
            Put (stdout, "  driven ");
            Put (stdout,
                 Grt.Processes.Get_Rti_Context (Sig.S.Drivers (I - 1).Proc));
            New_Line (stdout);
         end loop;
      end if;
   end Disp_Scalar_Sensitivity;

   procedure Disp_All_Sensitivity is
   begin
      Foreach_Scalar_Signal (Disp_Scalar_Sensitivity'access);
   end Disp_All_Sensitivity;


   --  Option disp-signals-map

   procedure Disp_Signals_Map_Scalar (Val_Addr : Address;
                                      Val_Name : Vstring;
                                      Val_Type : Ghdl_Rti_Access;
                                      Parent : Rti_Object)
   is
      pragma Unreferenced (Val_Type);

      function To_Ghdl_Signal_Ptr is new Ada.Unchecked_Conversion
        (Source => Address, Target => Ghdl_Signal_Ptr);

      S : Ghdl_Signal_Ptr;
   begin
      Disp_Signal_Name (stdout,
                        Parent.Ctxt, To_Ghdl_Rtin_Object_Acc (Parent.Obj));
      Put (stdout, Val_Name);
      Put (": ");
      S := To_Ghdl_Signal_Ptr (To_Addr_Acc (Val_Addr).all);
      Put (stdout, S.all'Address);
      Put (" net: ");
      Put_I32 (stdout, Ghdl_I32 (S.Net));
      if S.Has_Active then
         Put (" +A");
      end if;
      New_Line;
   end Disp_Signals_Map_Scalar;

   procedure Disp_Signals_Map is
   begin
      Foreach_Scalar_Signal (Disp_Signals_Map_Scalar'access);
   end Disp_Signals_Map;

   --  Option --disp-signals-table
   procedure Disp_Mode_Signal (Mode : Mode_Signal_Type)
   is
   begin
      case Mode is
         when Mode_Signal =>
            Put ("signal");
         when Mode_Linkage =>
            Put ("linkage");
         when Mode_Buffer =>
            Put ("buffer");
         when Mode_Out =>
            Put ("out");
         when Mode_Inout =>
            Put ("inout");
         when Mode_In =>
            Put ("in");
         when Mode_Stable =>
            Put ("stable");
         when Mode_Quiet =>
            Put ("quiet");
         when Mode_Transaction =>
            Put ("transaction");
         when Mode_Delayed =>
            Put ("delayed");
         when Mode_Guard =>
            Put ("guard");
         when Mode_Conv_In =>
            Put ("conv_in");
         when Mode_Conv_Out =>
            Put ("conv_out");
         when Mode_End =>
            Put ("end");
      end case;
   end Disp_Mode_Signal;

   procedure Disp_Signals_Table
   is
      Sig : Ghdl_Signal_Ptr;
   begin
      for I in Sig_Table.First .. Sig_Table.Last loop
         Sig := Sig_Table.Table (I);
         Put_Sig_Index (I);
         Put (": ");
         Put (stdout, Sig.all'Address);
         if Sig.Has_Active then
            Put (" +A");
         end if;
         Put (" net: ");
         Put_I32 (stdout,  Ghdl_I32 (Sig.Net));
         Put (" smode: ");
         Disp_Mode_Signal (Sig.S.Mode_Sig);
         Put (" #prt: ");
         Put_I32 (stdout, Ghdl_I32 (Sig.Nbr_Ports));
         if Sig.S.Mode_Sig in Mode_Signal_User then
            Put (" #drv: ");
            Put_I32 (stdout, Ghdl_I32 (Sig.S.Nbr_Drivers));
            case Sig.Flags.Sig_Kind is
               when Kind_Signal_No =>
                  Put ("  ");
               when Kind_Signal_Register =>
                  Put (" reg");
               when Kind_Signal_Bus =>
                  Put (" bus");
            end case;
            if Sig.S.Effective /= null then
               Put (" eff: ");
               Put (stdout, Sig.S.Effective.all'Address);
            end if;
            if Sig.S.Resolv /= null then
               Put (" resolved");
            end if;
         end if;
         if Boolean'(False) then
            Put (" link: ");
            Put (stdout, Sig.Link.all'Address);
         end if;
         New_Line;
         if Sig.Nbr_Ports /= 0 then
            for J in 1 .. Sig.Nbr_Ports loop
               Put ("  ");
               Put (stdout, Sig.Ports (J - 1).all'Address);
            end loop;
            New_Line;
         end if;
      end loop;
      Grt.Stdio.fflush (stdout);
   end Disp_Signals_Table;

   procedure Disp_A_Signal (Sig : Ghdl_Signal_Ptr) is
   begin
      Put_Signal_Name (stdout, Sig);
      Disp_Simple_Signal (Sig, null, True);
   end Disp_A_Signal;

   procedure Put_Signal_Name (Stream : FILEs; Sig : Ghdl_Signal_Ptr)
   is
      Found : Boolean := False;
      Cur_Ctxt : Rti_Context;
      Cur_Sig : Ghdl_Rtin_Object_Acc;

      procedure Process_Scalar  (Val_Addr : Address;
                                 Val_Name : Vstring;
                                 Val_Type : Ghdl_Rti_Access;
                                 Param : Boolean)
      is
         pragma Unreferenced (Val_Type);
         pragma Unreferenced (Param);
         Sig1 : Ghdl_Signal_Ptr;
      begin
         --  Read the signal.
         Sig1 := To_Ghdl_Signal_Ptr (To_Addr_Acc (Val_Addr).all);
         if Sig1 = Sig and not Found then
            Disp_Signal_Name (Stream, Cur_Ctxt, Cur_Sig);
            Put (Stream, Val_Name);
            Found := True;
         end if;
      end Process_Scalar;

      procedure Foreach_Scalar is new Grt.Rtis_Utils.Foreach_Scalar
        (Param_Type => Boolean, Process => Process_Scalar);

      function Process_Block (Ctxt : Rti_Context; Obj : Ghdl_Rti_Access)
                             return Traverse_Result is
      begin
         case Obj.Kind is
            when Ghdl_Rtik_Signal
              | Ghdl_Rtik_Port
              | Ghdl_Rtik_Guard
              | Ghdl_Rtik_Attribute_Stable
              | Ghdl_Rtik_Attribute_Quiet
              | Ghdl_Rtik_Attribute_Transaction =>
               Cur_Ctxt := Ctxt;
               Cur_Sig := To_Ghdl_Rtin_Object_Acc (Obj);
               Foreach_Scalar
                 (Ctxt, Cur_Sig.Obj_Type,
                  Loc_To_Addr (Cur_Sig.Common.Depth, Cur_Sig.Loc, Ctxt),
                  True, True);
               if Found then
                  return Traverse_Stop;
               end if;
            when others =>
               null;
         end case;
         return Traverse_Ok;
      end Process_Block;

      function Foreach_Block is new Grt.Rtis_Utils.Traverse_Blocks
        (Process_Block);

      Res_Status : Traverse_Result;
      pragma Unreferenced (Res_Status);

      Top : constant Rti_Context := Get_Top_Context;
   begin
      if Top /= Null_Context then
         Res_Status := Foreach_Block (Top);
      end if;

      if not Found then
         Put (Stream, "(unknown signal)");
      end if;
   end Put_Signal_Name;

end Grt.Disp_Signals;
