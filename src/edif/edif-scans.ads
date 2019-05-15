with Types; use Types;
with Edif.Tokens; use Edif.Tokens;

package Edif.Scans is
   Current_Token : Token_Type;
   Current_Identifier : Name_Id;
   Current_Number : Int32;
   Current_String : String8_Id;
   Current_String_Len : Uns32;

   --  Initialize the scanner with FILE.
   procedure Set_File (File : Source_File_Entry);

   --  Return the location of the token that has just been scaned.
   function Get_Token_Location return Location_Type;

   --  Scan the source file until the next token.
   procedure Scan;
end Edif.Scans;
