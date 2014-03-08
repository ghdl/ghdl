with Ada.Unchecked_Conversion;

with Ortho_Code.Binary;
with Binary_File; use Binary_File;
with Binary_File.Memory;

package body Ortho_Mcode.Jit is
   --  Set address of non-defined global variables or functions.
   procedure Set_Address (Decl : O_Dnode; Addr : Address)
   is
      use Ortho_Code.Binary;
   begin
      Binary_File.Memory.Set_Symbol_Address
        (Get_Decl_Symbol (Ortho_Code.O_Dnode (Decl)), Addr);
   end Set_Address;

   --  Get address of a global.
   function Get_Address (Decl : O_Dnode) return Address
   is
      use Ortho_Code.Binary;

      function Conv is new Ada.Unchecked_Conversion
        (Source => Pc_Type, Target => Address);
   begin
      return Conv (Get_Symbol_Vaddr
                     (Get_Decl_Symbol (Ortho_Code.O_Dnode (Decl))));
   end Get_Address;
end Ortho_Mcode.Jit;
