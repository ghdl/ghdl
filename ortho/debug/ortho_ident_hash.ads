package Ortho_Ident_Hash is
   type O_Ident is private;
   O_Ident_Nul : constant O_Ident;

   function Get_Identifier (Str : String) return O_Ident;
   function Get_String (Id : O_Ident) return String;
   function Is_Equal (L, R : O_Ident) return Boolean renames "=";
   function Is_Equal (Id : O_Ident; Str : String) return Boolean;
   function Is_Nul (Id : O_Ident) return Boolean;
private
   type Hash_Type is mod 2**32;

   type String_Acc is access constant String;

   --  Symbol table.
   type Ident_Type;
   type O_Ident is access Ident_Type;
   type Ident_type is record
      --  The hash for the symbol.
      Hash : Hash_Type;
      --  Identification of the symbol.
      Ident : String_Acc;
      --  Next symbol with the same collision.
      Next : O_Ident;
   end record;

   O_Ident_Nul : constant O_Ident := null;
end Ortho_Ident_Hash;
