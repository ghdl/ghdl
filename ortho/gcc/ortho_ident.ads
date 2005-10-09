with System; use System;

package Ortho_Ident is
   subtype O_Ident is Address;
   function Get_Identifier (Str : String) return O_Ident;
   function Get_String (Id : O_Ident) return String;
   function Is_Equal (L, R : O_Ident) return Boolean renames System."=";
   function Is_Equal (Id : O_Ident; Str : String) return Boolean;
   O_Ident_Nul : constant O_Ident;
private
   O_Ident_Nul : constant O_Ident := Null_Address;
end Ortho_Ident;
