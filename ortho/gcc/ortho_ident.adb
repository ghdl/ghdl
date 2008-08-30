package body Ortho_Ident is
   function Get_Identifier_With_Length (Str : Address; Size : Integer)
                                       return O_Ident;
   pragma Import (C, Get_Identifier_With_Length);

   function Compare_Identifier_String
     (Id : O_Ident; Str : Address; Size : Integer)
     return Boolean;
   pragma Import (C, Compare_Identifier_String);
   pragma Warnings (Off, Compare_Identifier_String);

   function Get_Identifier (Str : String) return O_Ident is
   begin
      return Get_Identifier_With_Length (Str'Address, Str'Length);
   end Get_Identifier;

   function Is_Equal (Id : O_Ident; Str : String) return Boolean is
   begin
      return Compare_Identifier_String (Id, Str'Address, Str'Length);
   end Is_Equal;

   function Get_String (Id : O_Ident) return String
   is
      procedure Get_Identifier_String
        (Id : O_Ident; Str_Ptr : Address; Len_Ptr : Address);
      pragma Import (C, Get_Identifier_String);

      Len : Natural;
      type Str_Acc is access String (Positive);
      Str : Str_Acc;
   begin
      Get_Identifier_String (Id, Str'Address, Len'Address);
      return Str (1 .. Len);
   end Get_String;

end Ortho_Ident;

