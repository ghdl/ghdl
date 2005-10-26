package Ortho_Ident_Simple is
   type O_Ident is private;
   O_Ident_Nul : constant O_Ident;

   function Get_Identifier (Str : String) return O_Ident;
   function Get_String (Id : O_Ident) return String;
   function Is_Equal (L, R : O_Ident) return Boolean renames "=";
   function Is_Equal (Id : O_Ident; Str : String) return Boolean;
   function Is_Nul (Id : O_Ident) return Boolean;
private
   type O_Ident is access String;
   O_Ident_Nul : constant O_Ident := null;
end Ortho_Ident_Simple;
