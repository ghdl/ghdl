with Interfaces.C; use Interfaces.C;

package body Foreigns is
   function Sin (Arg : double) return double;
   pragma Import (C, Sin);

   function Log (Arg : double) return double;
   pragma Import (C, Log);

   function Exp (Arg : double) return double;
   pragma Import (C, Exp);

   function Sqrt (Arg : double) return double;
   pragma Import (C, Sqrt);

   function Asin (Arg : double) return double;
   pragma Import (C, Asin);

   function Acos (Arg : double) return double;
   pragma Import (C, Acos);

   function Asinh (Arg : double) return double;
   pragma Import (C, Asinh);

   function Acosh (Arg : double) return double;
   pragma Import (C, Acosh);

   function Atanh (X : double) return double;
   pragma Import (C, Atanh);

   function Atan2 (X, Y : double) return double;
   pragma Import (C, Atan2);

   type String_Cacc is access constant String;
   type Foreign_Record is record
      Name : String_Cacc;
      Addr : Address;
   end record;


   Foreign_Arr : constant array (Natural range <>) of Foreign_Record :=
     (
      (new String'("sin"), Sin'Address),
      (new String'("log"), Log'Address),
      (new String'("exp"), Exp'Address),
      (new String'("sqrt"), Sqrt'Address),
      (new String'("asin"), Asin'Address),
      (new String'("acos"), Acos'Address),
      (new String'("asinh"), Asinh'Address),
      (new String'("acosh"), Acosh'Address),
      (new String'("atanh"), Atanh'Address),
      (new String'("atan2"), Atan2'Address)
     );

   function Find_Foreign (Name : String) return Address is
   begin
      for I in Foreign_Arr'Range loop
         if Foreign_Arr(I).Name.all = Name then
            return Foreign_Arr(I).Addr;
         end if;
      end loop;
      return Null_Address;
   end Find_Foreign;
end Foreigns;
