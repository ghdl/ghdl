--  GHDL Run Time (GRT) - VHPI implementation.
--  Copyright (C) 2021 Marlon James
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
with Ada.Unchecked_Conversion;
with Grt.Astdio; use Grt.Astdio;
with Grt.Errors; use Grt.Errors;
with Grt.Hooks; use Grt.Hooks;
with Grt.Stdio; use Grt.Stdio;
with Grt.Vstrings; use Grt.Vstrings;

package body Grt.Vhpi is
   --  The VHPI interface requires libdl (dlopen, dlsym) to be linked in.
   --  This is now set in Makefile, since this is target dependent.
   --  pragma Linker_Options ("-ldl");

   --  If true, emit traces
   Flag_Trace : Boolean := False;
   Trace_File : FILEs;
   pragma Unreferenced (Flag_Trace);

   VhpiUndefined_External : constant Integer := -1;

   ----------------------------------------------------------------------------
   -- Internal helper functions
   ----------------------------------------------------------------------------

   -- VHPI errors

   Default_Message : constant String := "(no error message)" & NUL;

   Err_Severity : VhpiSeverityT := VhpiNote;
   Err_Message : Ghdl_C_String := To_Ghdl_C_String (Default_Message'Address);
   Err_Str : Ghdl_C_String := null;
   Err_File : Ghdl_C_String := null;
   Err_Line : Integer_32 := -1;
   Err_Occured : Boolean := False;

   Buf_Err_Message : Vstring;

   procedure Reset_Error is
   begin
      Err_Severity := VhpiNote;
      Err_Message := To_Ghdl_C_String (Default_Message'Address);
      Err_Str := null;
      Err_File := null;
      Err_Line := -1;
      Err_Occured := False;
   end Reset_Error;

   Err_Str_S : constant String := "GHDL Error ";
   -- VHPI function not implemented
   VHPI_0001_Str  : constant String := Err_Str_S & "VHPI_0001";

   procedure Set_Err_Str (S : String) is
   begin
      Err_Str := To_Ghdl_C_String (S'Address);
   end Set_Err_Str;

   procedure Error_Unimplimented (Name : String) is
   begin
      Err_Severity := VhpiError;
      Reset (Buf_Err_Message);
      Append (Buf_Err_Message, Name);
      Append (Buf_Err_Message, " not implemented");
      Append (Buf_Err_Message, NUL);
      Err_Message := Get_C_String (Buf_Err_Message);
      Set_Err_Str (VHPI_0001_Str);
      Err_Occured := True;
   end Error_Unimplimented;

   -- Internal implementations for variadic functions in grt-cvhpi.c

   function Vhpi_Assert_Internal (Severity: Integer; Msg : Ghdl_C_String)
                                 return Integer
   is
      pragma Unreferenced (Severity);
      pragma Unreferenced (Msg);
   begin
      Reset_Error;
      Error_Unimplimented ("vhpi_assert");
      return 0;
   end Vhpi_Assert_Internal;

   function Vhpi_Control_Internal (Command : VhpiSimControlT; Status : Integer)
                                  return Integer
   is
      pragma Unreferenced (Command);
      pragma Unreferenced (Status);
   begin
      Reset_Error;
      Error_Unimplimented ("vhpi_control");
      return 0;
   end Vhpi_Control_Internal;

   ----------------------------------------------------------------------------
   -- VHPI functions
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   -- Callback related

   -- vhpiHandleT vhpi_register_cb (vhpiCbDataT *cb_data_p, int32_t flags)
   function vhpi_register_cb (Data : VhpiCbData_Access; Flags : Callback_Flags)
                             return Vhpi_External_Handle
   is
      pragma Unreferenced (Data);
      pragma Unreferenced (Flags);
   begin
      Reset_Error;
      Error_Unimplimented ("vhpi_register_cb");
      return null;
   end vhpi_register_cb;

   -- int vhpi_remove_cb (vhpiHandleT cb_obj)
   function vhpi_remove_cb (Cb : Vhpi_External_Handle) return Integer
   is
      pragma Unreferenced (Cb);
   begin
      Reset_Error;
      Error_Unimplimented ("vhpi_remove_cb");
      return 0;
   end vhpi_remove_cb;

   -- int vhpi_disable_cb (vhpiHandleT cb_obj)
   function vhpi_disable_cb (Cb : Vhpi_External_Handle) return Integer
   is
      pragma Unreferenced (Cb);
   begin
      Reset_Error;
      Error_Unimplimented ("vhpi_disable_cb");
      return 0;
   end vhpi_disable_cb;

   -- int vhpi_enable_cb (vhpiHandleT cb_obj)
   function vhpi_enable_cb (Cb : Vhpi_External_Handle) return Integer
   is
      pragma Unreferenced (Cb);
   begin
      Reset_Error;
      Error_Unimplimented ("vhpi_enable_cb");
      return 0;
   end vhpi_enable_cb;

   -- int vhpi_get_cb_info (vhpiHandleT object, vhpiCbDataT *cb_data_p)
   function vhpi_get_cb_info
     (Obj : Vhpi_External_Handle; Data : VhpiCbData_Access) return Integer
   is
      pragma Unreferenced (Obj);
      pragma Unreferenced (Data);
   begin
      Reset_Error;
      Error_Unimplimented ("vhpi_get_cb_info");
      return 0;
   end vhpi_get_cb_info;

   -- For obtaining handles

   -- vhpiHandleT vhpi_handle_by_name (const char *name, vhpiHandleT scope)
   function vhpi_handle_by_name
     (Name : Ghdl_C_String; Scope : Vhpi_External_Handle)
     return Vhpi_External_Handle
   is
      pragma Unreferenced (Name);
      pragma Unreferenced (Scope);
   begin
      Reset_Error;
      Error_Unimplimented ("vhpi_handle_by_name");
      return null;
   end vhpi_handle_by_name;

   -- vhpiHandleT vhpi_handle_by_index (vhpiOneToManyT itRel,
   --                                   vhpiHandleT parent, int32_t indx)
   function vhpi_handle_by_index
     (Rel : Integer; Parent : Vhpi_External_Handle; Index: Integer)
     return Vhpi_External_Handle
   is
      pragma Unreferenced (Rel);
      pragma Unreferenced (Parent);
      pragma Unreferenced (Index);
   begin
      Reset_Error;
      Error_Unimplimented ("vhpi_handle_by_index");
      return null;
   end vhpi_handle_by_index;

   -- For traversing relationships

   -- vhpiHandleT vhpi_handle (vhpiOneToOneT type, vhpiHandleT referenceHandle)
   function vhpi_handle (Rel: Integer; Ref: Vhpi_External_Handle)
                        return Vhpi_External_Handle
   is
      pragma Unreferenced (Rel);
      pragma Unreferenced (Ref);
   begin
      Reset_Error;
      Error_Unimplimented ("vhpi_handle");
      return null;
   end vhpi_handle;

   -- vhpiHandleT vhpi_iterator (vhpiOneToManyT type,
   --                            vhpiHandleT referenceHandle)
   function vhpi_iterator (Rel: Integer; Ref: Vhpi_External_Handle)
                          return Vhpi_External_Handle
   is
      pragma Unreferenced (Rel);
      pragma Unreferenced (Ref);
   begin
      Reset_Error;
      Error_Unimplimented ("vhpi_iterator");
      return null;
   end vhpi_iterator;

   -- vhpiHandleT vhpi_scan (vhpiHandleT iterator)
   function vhpi_scan (Iter : Vhpi_External_Handle) return Vhpi_External_Handle
   is
      pragma Unreferenced (Iter);
   begin
      Reset_Error;
      Error_Unimplimented ("vhpi_scan");
      return null;
   end vhpi_scan;

   -- For processing properties

   -- vhpiIntT vhpi_get (vhpiIntPropertyT property, vhpiHandleT object)
   function vhpi_get (Property: Integer; Ref: Vhpi_External_Handle)
                     return VhpiIntT
   is
      pragma Unreferenced (Property);
      pragma Unreferenced (Ref);
   begin
      Reset_Error;
      Error_Unimplimented ("vhpi_get");
      return VhpiIntT (VhpiUndefined_External);
   end vhpi_get;

   -- const vhpiCharT * vhpi_get_str (vhpiStrPropertyT property,
   --                                 vhpiHandleT object)
   function vhpi_get_str (Property : Integer; Ref : Vhpi_External_Handle)
                         return Ghdl_C_String
   is
      pragma Unreferenced (Property);
      pragma Unreferenced (Ref);
   begin
      Reset_Error;
      Error_Unimplimented ("vhpi_get_str");
      return null;
   end vhpi_get_str;

   -- vhpiRealT vhpi_get_real (vhpiRealPropertyT property, vhpiHandleT object)
   function vhpi_get_real (Property : Integer; Ref : Vhpi_External_Handle)
                          return Ghdl_Real
   is
      pragma Unreferenced (Property);
      pragma Unreferenced (Ref);
   begin
      Reset_Error;
      Error_Unimplimented ("vhpi_get_real");
      return 0.0;
   end vhpi_get_real;

   -- vhpiPhysT vhpi_get_phys (vhpiPhysPropertyT property, vhpiHandleT object)
   function vhpi_get_phys (Property : Integer; Ref : Vhpi_External_Handle)
                          return VhpiPhysT
   is
      pragma Unreferenced (Property);
      pragma Unreferenced (Ref);

      Res : constant VhpiPhysT := (0, 0);
   begin
      Reset_Error;
      Error_Unimplimented ("vhpi_get_phys");
      return Res;
   end vhpi_get_phys;

   -- For access to protected types

   -- int vhpi_protected_call (vhpiHandleT varHdl,
   --                          vhpiUserFctT userFct,
   --                          void *userData)
   function vhpi_protected_call (Var : Vhpi_External_Handle;
                                 User_Fun : VhpiUserFctT;
                                 User_Data : System.Address)
                                return Integer
   is
      pragma Unreferenced (Var);
      pragma Unreferenced (User_Fun);
      pragma Unreferenced (User_Data);
   begin
      Reset_Error;
      Error_Unimplimented ("vhpi_protected_call");
      return 0;
   end vhpi_protected_call;

   -- For value processing

   -- int vhpi_get_value (vhpiHandleT expr, vhpiValueT *value_p)
   function vhpi_get_value
     (Expr : Vhpi_External_Handle; Value : VhpiValue_Access) return Integer
   is
      pragma Unreferenced (Expr);
      pragma Unreferenced (Value);
   begin
      Reset_Error;
      Error_Unimplimented ("vhpi_get_value");
      return 0;
   end vhpi_get_value;

   -- int vhpi_put_value (vhpiHandleT object,
   --                     vhpiValueT *value_p,
   --                     vhpiPutValueModeT mode)
   function vhpi_put_value (Obj : Vhpi_External_Handle;
                            Value : VhpiValue_Access;
                            Mode : VhpiPutValueModeT)
                           return Integer
   is
      pragma Unreferenced (Obj);
      pragma Unreferenced (Value);
      pragma Unreferenced (Mode);
   begin
      Reset_Error;
      Error_Unimplimented ("vhpi_put_value");
      return 0;
   end vhpi_put_value;

   -- int vhpi_schedule_transaction (vhpiHandleT drivHdl,
   --                                vhpiValueT *value_p,
   --                                uint32_t numValues,
   --                                vhpiTimeT *delayp,
   --                                vhpiDelayModeT delayMode,
   --                                vhpiTimeT *pulseRejp)
   function vhpi_schedule_transaction (Driver : Vhpi_External_Handle;
                                       Value : VhpiValue_Access;
                                       Num_Values : Unsigned_32;
                                       Delay_Value : VhpiTime_Access;
                                       Delay_Mode : VhpiDelayModeT;
                                       Pulse_Rejection : VhpiTime_Access)
                                      return Integer
   is
      pragma Unreferenced (Driver);
      pragma Unreferenced (Value);
      pragma Unreferenced (Num_Values);
      pragma Unreferenced (Delay_Value);
      pragma Unreferenced (Delay_Mode);
      pragma Unreferenced (Pulse_Rejection);
   begin
      Reset_Error;
      Error_Unimplimented ("vhpi_schedule_transaction");
      return 0;
   end vhpi_schedule_transaction;

   -- int vhpi_format_value (const vhpiValueT *in_value_p,
   --                        vhpiValueT *out_value_p)
   function vhpi_format_value
     (In_Val : VhpiValue_Access; Out_Val : VhpiValue_Access) return Integer
   is
      pragma Unreferenced (In_Val);
      pragma Unreferenced (Out_Val);
   begin
      Reset_Error;
      Error_Unimplimented ("vhpi_format_value");
      return 0;
   end vhpi_format_value;

   -- For time processing

   -- void vhpi_get_time (vhpiTimeT *time_p, long *cycles)
   procedure vhpi_get_time (Time : VhpiTime_Access; Cycles : access Integer)
   is
      pragma Unreferenced (Time);
      pragma Unreferenced (Cycles);
   begin
      Reset_Error;
      Error_Unimplimented ("vhpi_get_time");
      null;
   end vhpi_get_time;

   -- int vhpi_get_next_time (vhpiTimeT *time_p)
   function vhpi_get_next_time (Time : VhpiTime_Access) return Integer
   is
      pragma Unreferenced (Time);
   begin
      Reset_Error;
      Error_Unimplimented ("vhpi_get_next_time");
      return 0;
   end vhpi_get_next_time;

   -- Utilities to print VHDL strings

   -- int vhpi_is_printable ( char ch )
   function vhpi_is_printable (Ch : Character) return Integer
   is
      pragma Unreferenced (Ch);
   begin
      Reset_Error;
      Error_Unimplimented ("vhpi_is_printable");
      return 0;
   end vhpi_is_printable;

   -- Utility routines

   -- int vhpi_compare_handles (vhpiHandleT handle1, vhpiHandleT handle2)
   function vhpi_compare_handles (Hdl1, Hdl2 : Vhpi_External_Handle)
                                 return Integer
   is
      pragma Unreferenced (Hdl1);
      pragma Unreferenced (Hdl2);
   begin
      Reset_Error;
      Error_Unimplimented ("vhpi_compare_handles");
      return 0;
   end vhpi_compare_handles;

   -- int vhpi_check_error (vhpiErrorInfoT *error_info_p)
   function vhpi_check_error (Info : VhpiErrorInfo_Access) return Integer
   is
      function To_Integer (B : Boolean) return Integer is
      begin
         if B then
            return 1;
         else
            return 0;
         end if;
      end To_Integer;
   begin
      if Info /= null then
         Info.all := (Severity => Err_Severity,
                      Msg => Err_Message,
                      Str => Err_Str,
                      File => Err_File,
                      Line => Err_Line);
      end if;
      return To_Integer (Err_Occured);
   end vhpi_check_error;

   -- int vhpi_release_handle (vhpiHandleT object)
   function vhpi_release_handle (Obj : Vhpi_External_Handle) return Integer
   is
      pragma Unreferenced (Obj);
   begin
      Reset_Error;
      Error_Unimplimented ("vhpi_release_handle");
      return 0;
   end vhpi_release_handle;

   -- Creation functions
   -- vhpiHandleT vhpi_create (vhpiClassKindT kind,
   --                          vhpiHandleT handle1,
   --                          vhpiHandleT handle2)
   function vhpi_create (Kind : Integer; Hdl1, Hdl2 : Vhpi_External_Handle)
                        return Vhpi_External_Handle
   is
      pragma Unreferenced (Kind);
      pragma Unreferenced (Hdl1);
      pragma Unreferenced (Hdl2);
   begin
      Reset_Error;
      Error_Unimplimented ("vhpi_create");
      return null;
   end vhpi_create;

   -- Foreign model data structures and functions

   -- vhpiHandleT vhpi_register_foreignf (vhpiForeignDataT *foreignDatap)
   function vhpi_register_foreignf (Data : VhpiForeignData_Access)
                                   return Vhpi_External_Handle
   is
      pragma Unreferenced (Data);
   begin
      Reset_Error;
      Error_Unimplimented ("vhpi_register_foreignf");
      return null;
   end vhpi_register_foreignf;

   -- int vhpi_get_foreignf_info (vhpiHandleT hdl,
   --                             vhpiForeignDataT *foreignDatap)
   function vhpi_get_foreignf_info
     (Hdl : Vhpi_External_Handle; Data : VhpiForeignData_Access)
     return Integer
   is
      pragma Unreferenced (Hdl);
      pragma Unreferenced (Data);
   begin
      Reset_Error;
      Error_Unimplimented ("vhpi_get_foreignf_info");
      return 0;
   end vhpi_get_foreignf_info;

   -- For saving and restoring foreign models data

   -- size_t vhpi_get_data (int32_t id, void *dataLoc, size_t numBytes);
   function vhpi_get_data
     (Id : Integer_32; Data_Loc : System.Address; Num_Bytes : size_t)
     return size_t
   is
      pragma Unreferenced (Id);
      pragma Unreferenced (Data_Loc);
      pragma Unreferenced (Num_Bytes);
   begin
      Reset_Error;
      Error_Unimplimented ("vhpi_get_data");
      return 0;
   end vhpi_get_data;

   -- size_t vhpi_put_data (int32_t id, void *dataLoc, size_t numBytes);
   function vhpi_put_data
     (Id : Integer_32; Data_Loc : System.Address; Num_Bytes : size_t)
     return size_t
   is
      pragma Unreferenced (Id);
      pragma Unreferenced (Data_Loc);
      pragma Unreferenced (Num_Bytes);
   begin
      Reset_Error;
      Error_Unimplimented ("vhpi_put_data");
      return 0;
   end vhpi_put_data;


   ----------------------------------------------------------------------------
   -- GHDL hooks
   ----------------------------------------------------------------------------

   type Lib_Cell;
   type Lib_Access is access Lib_Cell;

   type Lib_Cell is record
      File_Name : String_Access;
      Func_Name : String_Access;
      Next : Lib_Access;
   end record;

   Vhpi_Libraries : Lib_Access := null;

   procedure Vhpi_Help is
   begin
      Put_Line
        (" --vhpi=FILENAME[:ENTRYPOINT]  load VHPI library, optionally");
      Put_Line
        ("                               provide entry point name");
      Put_Line
        (" --vhpi-trace[=FILE]  trace vhpi calls to stdout or provided FILE");
   end Vhpi_Help;

   ------------------------------------------------------------------------
   --  Return TRUE if OPT is an option for VHPI.
   function Vhpi_Option (Opt : String) return Boolean
   is
      F : constant Natural := Opt'First;

      procedure Bad_Option is
      begin
         Error_S ("incorrect option '");
         Diag_C (Opt);
         Error_E ("'");
      end Bad_Option;
   begin
      if Opt'Length < 6 or else Opt (F .. F + 5) /= "--vhpi" then
         return False;
      end if;
      if Opt'Length = 7 then
         Bad_Option;
         return False;
      end if;
      if Opt'Length > 7 and then Opt (F + 6) = '=' then
         -- Need to support Windows path names and optional entrypoint.
         -- Valid examples:
         -- C:\vhpi_lib.dll:entry_func
         -- .\vhpi_lib.dll:entry_func
         -- ./vhpi_lib.so:entry_func
         -- /path/to/vhpi_lib:entry_func
         -- vhpi_lib:entry_func
         declare
            P : Natural;
            Lf, Ll, L_Len : Natural;
            Ef, El, E_Len : Natural;
            Lib : Lib_Access;
            File : String_Access;
            Func : String_Access;
         begin
            P := F + 7;
            -- Extract library.
            Lf := P;
            while P <= Opt'Length and then Opt (P) /= ':' loop
               P := P + 1;
            end loop;
            -- Skip colon after volume/drive letter on Windows.
            -- This will break if library path is one character.
            if P <= Opt'Length and then Opt (P) = ':' and then P = 2 then
               while P <= Opt'Length and then Opt (P) /= ':' loop
                  P := P + 1;
               end loop;
            end if;
            Ll := P - 1;

            -- Extract entrypoint.
            Ef := P + 1;
            El := Opt'Length;

            -- Store library info.
            Lib := new Lib_Cell;
            --  Add an extra NUL character.
            L_Len := Ll - Lf + 2;
            File := new String (1 .. L_Len);
            File (1 .. L_Len - 1) := Opt (Lf .. Ll);
            File (File'Last) := NUL;
            Lib.File_Name := File;
            if Ef <= El then
               --  Add an extra NUL character.
               E_Len := El - Ef + 2;
               Func := new String (1 .. E_Len);
               Func (1 .. E_Len - 1) := Opt (Ef .. El);
               Func (Func'Last) := NUL;
               Lib.Func_Name := Func;
            end if;

            -- Add new library to the list.
            if Vhpi_Libraries = null then
               Vhpi_Libraries := Lib;
            else
               declare
                  L : Lib_Access := Vhpi_Libraries;
               begin
                  while L.Next /= null loop
                     L := L.Next;
                  end loop;
                  L.Next := Lib;
               end;
            end if;
         end;
         return True;
      elsif Opt'Length >= 12 and then Opt (F + 6 .. F + 11) = "-trace" then
         if Opt'Length > 12 and then Opt (F + 12) = '=' then
            declare
               Filename : String (1 .. Opt'Length - 12);
               Mode : constant String := "wt" & NUL;
            begin
               Filename (1 .. Filename'Last - 1) := Opt (F + 13 .. Opt'Last);
               Filename (Filename'Last) := NUL;
               Trace_File := fopen (Filename'Address, Mode'Address);
               if Trace_File = NULL_Stream then
                  Error_S ("cannot open vhpi trace file '");
                  Diag_C (Opt (F + 13 .. Opt'Last));
                  Error_E ("'");
                  return False;
               end if;
            end;
         elsif Opt'Length = 12 then
            Trace_File := stdout;
         else
            Bad_Option;
            return False;
         end if;
         Flag_Trace := True;
         return True;
      else
         return False;
      end if;
   end Vhpi_Option;

   ------------------------------------------------------------------------
   --  Called before elaboration.

   -- int loadVhpiModule (const char* libname, const char* entrypoint)
   function LoadVhpiModule (Filename, Funcname: Address) return Integer;
   pragma Import (C, LoadVhpiModule, "loadVhpiModule");

   procedure Vhpi_Init
   is
      Lib : Lib_Access := Vhpi_Libraries;
      Res : Integer;
   begin
      if Lib = null then
         return;
      end if;
      while Lib /= null loop
         if Lib.Func_Name = null then
            Res := LoadVhpiModule
              (Lib.File_Name.all'Address, Null_Address);
         else
            Res := LoadVhpiModule
              (Lib.File_Name.all'Address, Lib.Func_Name.all'Address);
         end if;

         if Res /= 0 then
            Error_S ("cannot load VHPI module '");
            Diag_C (Lib.File_Name.all);
            if Lib.Func_Name /= null then
               Diag_C ("' with entry point '");
               Diag_C (Lib.Func_Name.all);
            end if;
            Error_E ("'");
         end if;
         Lib := Lib.Next;
      end loop;
   end Vhpi_Init;

   ------------------------------------------------------------------------
   --  Called after elaboration.
   procedure Vhpi_Start is
   begin
      if Vhpi_Libraries = null then
         return;
      end if;

      -- Grt.Rtis_Types.Search_Types_RTI;
      -- Execute_Callback_List (VhpiCbStartOfSimulation_List);
   end Vhpi_Start;

   ------------------------------------------------------------------------
   --  Called at the end of the simulation.
   procedure Vhpi_End is
   begin
      -- Execute_Callback_List (VhpiCbEndOfSimulation_List);
      Free (Buf_Err_Message);
      null;
   end Vhpi_End;

   Vhpi_Hooks : aliased constant Hooks_Type :=
     (Desc => new String'("vhpi: vhpi compatible API"),
      Option => Vhpi_Option'Access,
      Help => Vhpi_Help'Access,
      Init => Vhpi_Init'Access,
      Start => Vhpi_Start'Access,
      Finish => Vhpi_End'Access);

   procedure Register is
   begin
      Register_Hooks (Vhpi_Hooks'Access);
   end Register;

end Grt.Vhpi;
