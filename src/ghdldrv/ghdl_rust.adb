with Ada.Unchecked_Deallocation;

package body Ghdl_Rust is
   function New_String_Acc_Record (Len : Natural)
                                  return String_Acc_Record_Acc is
   begin
      return new String_Acc_Record(Len);
   end New_String_Acc_Record;
   
   procedure Set_String_Acc_Record (Rec : String_Acc_Record_Acc;
                                    Pos : Positive;
                                    Str : Thin_String_Ptr;
                                    Len : Natural) is
   begin
      Rec.Str (Pos) := new String'(Str (1 .. Len));
   end Set_String_Acc_Record;
   
   procedure Free_String_Acc_Record (Rec : String_Acc_Record_Acc)
   is
      procedure Deallocate is new Ada.Unchecked_Deallocation
        (String_Acc_Record, String_Acc_Record_Acc);
      Dup : String_Acc_Record_Acc;
   begin
      for I in Rec.Str'Range loop
         Free (Rec.Str (I));
      end loop;
      Dup := Rec;
      Deallocate (Dup);
   end Free_String_Acc_Record;

end Ghdl_Rust;
