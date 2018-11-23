package Files_Map.Editor is
   procedure Replace_Text (File : Source_File_Entry;
                           Start_Line : Positive;
                           Start_Off  : Natural;
                           End_Line   : Positive;
                           End_Off    : Natural;
                           Text : String);

   procedure Replace_Text_Ptr (File : Source_File_Entry;
                               Start_Line : Positive;
                               Start_Off  : Natural;
                               End_Line   : Positive;
                               End_Off    : Natural;
                               Str : Thin_String_Ptr;
                               Str_Len : Natural);

   --  Set the position of the GAP in FILE.
   procedure Set_Gap (File : Source_File_Entry;
                      First : Source_Ptr;
                      Last : Source_Ptr);
end Files_Map.Editor;
