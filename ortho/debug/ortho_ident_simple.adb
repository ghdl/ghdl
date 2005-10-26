package body Ortho_Ident_Simple is
   function Get_Identifier (Str : String) return O_Ident
   is
   begin
      return new String'(Str);
   end Get_Identifier;

   function Get_String (Id : O_Ident) return String is
   begin
      if Id = null then
         return "?ANON?";
      else
         return Id.all;
      end if;
   end Get_String;

   function Is_Nul (Id : O_Ident) return Boolean is
   begin
      return Id = null;
   end Is_Nul;

   function Is_Equal (Id : O_Ident; Str : String) return Boolean is
   begin
      return Id.all = Str;
   end Is_Equal;
end Ortho_Ident_Simple;
