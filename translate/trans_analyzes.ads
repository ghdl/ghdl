with Iirs; use Iirs;

package Trans_Analyzes is
   --  Extract a list of drivers from PROC.
   function Extract_Drivers (Proc : Iir) return Iir_List;

   --  Free the list.
   procedure Free_Drivers_List (List : in out Iir_List);

   --  Dump list of drivers (LIST) for process PROC.
   procedure Dump_Drivers (Proc : Iir; List : Iir_List);

end Trans_Analyzes;
