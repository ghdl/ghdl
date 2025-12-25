with System; use System;
with Types;
with Std_Names;
with Flags;

with Trans_Foreign;
with Ortho_Jit;
with Grt.Lib;
with Grt.Files_Lib;
with Grt.Asserts;

with Vhdl.Errors; use Vhdl.Errors;
with Vhdl.Back_End; use Vhdl.Back_End;

package body Trans_Foreign_Jit is
   procedure Def (Decl : O_Dnode; Addr : Address)
     renames Ortho_Jit.Set_Address;

   function Get_Intrinsic_Address (Decl : Iir) return Address
   is
      use Types;
      use Std_Names;
      Id : constant Name_Id := Get_Identifier (Decl);
   begin
      case Id is
         when Name_Untruncated_Text_Read =>
            if Flags.Flag_Integer_64 then
               return Grt.Files_Lib.Ghdl_Untruncated_Text_Read_64'Address;
            else
               return Grt.Files_Lib.Ghdl_Untruncated_Text_Read_32'Address;
            end if;
         when Name_Textio_Read_Real =>
            return Grt.Lib.Textio_Read_Real'Address;
         when Name_Textio_Write_Real =>
            return Grt.Lib.Textio_Write_Real'Address;
         when Name_Control_Simulation =>
            return Grt.Lib.Ghdl_Control_Simulation'Address;
         when Name_Get_Resolution_Limit =>
            return Grt.Lib.Ghdl_Get_Resolution_Limit'Address;
         when Name_Get_Assert_Count =>
            return Grt.Asserts.Ghdl_Get_Assert_Count'Address;
         when Name_Clear_Assert_Count =>
            return Grt.Asserts.Ghdl_Clear_Assert_Count'Address;
         when others =>
            Error_Msg_Sem (+Decl, "unknown foreign intrinsic %i", +Decl);
            return Null_Address;
      end case;
   end Get_Intrinsic_Address;

   procedure Foreign_Hook (Decl : Iir;
                           Info : Foreign_Info_Type;
                           Ortho : O_Dnode)
   is
      Res : Address;
   begin
      case Info.Kind is
         when Foreign_Vhpidirect =>
            Res := Trans_Foreign.Get_Foreign_Address (Decl, Info);
         when Foreign_Intrinsic =>
            Res := Get_Intrinsic_Address (Decl);
         when Foreign_Unknown =>
            return;
      end case;
      if Res /= Null_Address then
         Def (Ortho, Res);
      end if;
   end Foreign_Hook;

   procedure Init is
   begin
      Trans_Foreign.Init;
   end Init;
end Trans_Foreign_Jit;
