with Hash;
with Interning;
with Name_Table;

with Foreigns;

with Vhdl.Errors; use Vhdl.Errors;

with Grt.Types; use Grt.Types;
with Grt.Dynload; use Grt.Dynload;
with Grt.Lib;
with Grt.Files_Lib;

package body Trans_Foreign is
   --  Elaboration mode.
   type Shlib_Object_Type is record
      Name : String_Access;
      Handler : Address;
   end record;

   function Shlib_Build (Name : String) return Shlib_Object_Type
   is
      Name_Acc : constant String_Access := new String'(Name);
      C_Name : constant String := Name & NUL;
      Handler : Address;
   begin
      Handler :=
        Grt_Dynload_Open (Grt.Types.To_Ghdl_C_String (C_Name'Address));
      return (Name => Name_Acc,
              Handler => Handler);
   end Shlib_Build;

   function Shlib_Equal (Obj : Shlib_Object_Type; Param : String)
                        return Boolean is
   begin
      return Obj.Name.all = Param;
   end Shlib_Equal;

   package Shlib_Interning is new Interning
     (Key_Type => String,
      Object_Type => Shlib_Object_Type,
      Hash => Hash.String_Hash,
      Build => Shlib_Build,
      Equal => Shlib_Equal);

   function Get_Foreign_Address
     (Decl : Iir; Info : Vhdl.Back_End.Foreign_Info_Type) return Address
   is
      use Vhdl.Back_End;
      Res : Address;
   begin
      case Info.Kind is
         when Foreign_Vhpidirect =>
            declare
               Name : constant String :=
                 Info.Subprg_Name (1 .. Info.Subprg_Len);
               Lib : constant String :=
                 Info.Lib_Name (1 .. Info.Lib_Len);
               Shlib : Shlib_Object_Type;
            begin
               if Info.Lib_Len = 0
                 or else Lib = "null"
               then
                  Res := Foreigns.Find_Foreign (Name);
                  if Res = Null_Address then
                     Error_Msg_Sem
                       (+Decl, "unknown foreign VHPIDIRECT '" & Name & "'");
                     return Null_Address;
                  end if;
               else
                  Shlib := Shlib_Interning.Get (Lib);
                  if Shlib.Handler = Null_Address then
                     Error_Msg_Sem
                       (+Decl, "cannot load VHPIDIRECT shared library '" &
                          Lib & "'");
                     return Null_Address;
                  end if;

                  declare
                     C_Name : constant String := Name & NUL;
                  begin
                     Res := Grt_Dynload_Symbol
                       (Shlib.Handler,
                        Grt.Types.To_Ghdl_C_String (C_Name'Address));
                  end;
                  if Res = Null_Address then
                     Error_Msg_Sem
                       (+Decl, "cannot resolve VHPIDIRECT symbol '"
                          & Name & "'");
                     return Null_Address;
                  end if;
               end if;
               return Res;
            end;
         when Foreign_Intrinsic =>

            declare
               Name : constant String :=
                 Name_Table.Image (Get_Identifier (Decl));
            begin
               if Name = "untruncated_text_read" then
                  Res := Grt.Files_Lib.Ghdl_Untruncated_Text_Read'Address;
               elsif Name = "textio_read_real" then
                  Res := Grt.Lib.Textio_Read_Real'Address;
               elsif Name = "textio_write_real" then
                  Res := Grt.Lib.Textio_Write_Real'Address;
               elsif Name = "control_simulation" then
                  Res := Grt.Lib.Ghdl_Control_Simulation'Address;
               elsif Name = "get_resolution_limit" then
                  Res := Grt.Lib.Ghdl_Get_Resolution_Limit'Address;
               else
                  Error_Msg_Sem
                    (+Decl, "unknown foreign intrinsic %i", +Decl);
                  Res := Null_Address;
               end if;
            end;
         when Foreign_Unknown =>
            null;
      end case;
      return Res;
   end Get_Foreign_Address;

   procedure Init is
   begin
      Shlib_Interning.Init;
   end Init;
end Trans_Foreign;
