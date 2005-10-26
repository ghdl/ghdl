package body Ortho_Ident_Hash is
   type O_Ident_Array is array (Hash_Type range <>) of O_Ident;
   Hash_Max : constant Hash_Type := 511;
   Symtable : O_Ident_Array (0 .. Hash_Max - 1) := (others => null);

   function Get_Identifier (Str : String) return O_Ident
   is
      Hash : Hash_Type;
      Ent : Hash_Type;
      Res : O_Ident;
   begin
      --  1.  Compute Hash.
      Hash := 0;
      for I in Str'Range loop
         Hash := Hash * 31 + Character'Pos (Str (I));
      end loop;

      --  2.  Search.
      Ent := Hash mod Hash_Max;
      Res := Symtable (Ent);
      while Res /= null loop
         if Res.Hash = Hash and then Res.Ident.all = Str then
            return Res;
         end if;
         Res := Res.Next;
      end loop;

      --  Not found: add.
      Res := new Ident_Type'(Hash => Hash,
                             Ident => new String'(Str),
                             Next => Symtable (Ent));
      Symtable (Ent) := Res;
      return Res;
   end Get_Identifier;

   function Get_String (Id : O_Ident) return String is
   begin
      if Id = null then
         return "?ANON?";
      else
         return Id.Ident.all;
      end if;
   end Get_String;

   function Is_Nul (Id : O_Ident) return Boolean is
   begin
      return Id = null;
   end Is_Nul;

   function Is_Equal (Id : O_Ident; Str : String) return Boolean is
   begin
      return Id.Ident.all = Str;
   end Is_Equal;
end Ortho_Ident_Hash;
