with System; use System;

package Ortho_Mcode.Jit is
   --  Set address of non-defined global variables or functions.
   procedure Set_Address (Decl : O_Dnode; Addr : Address);

   --  Get address of a global.
   function Get_Address (Decl : O_Dnode) return Address;
end Ortho_Mcode.Jit;
