with Grt.Types; use Grt.Types;

package Mhdlsim is
   function Process_Param (Opt : Ghdl_C_String; Len : Natural)
                          return Integer;
   pragma Export (C, Process_Param, "mhdlsim_vhdl_process_param");

   procedure Analyze_Init;
   pragma Export (C, Analyze_Init, "mhdlsim_vhdl_analyze_init");

   function Analyze_File (File : Ghdl_C_String; Len : Natural)
                    return Integer;
   pragma Export (C, Analyze_File, "mhdlsim_vhdl_analyze_file");

   function Known_Top_Unit return Integer;
   pragma Export (C, Known_Top_Unit, "mhdlsim_vhdl_known_top_unit");

   procedure Elaborate;
   pragma Export (C, Elaborate, "mhdlsim_vhdl_elaborate");

   procedure Run;
   pragma Export (C, Run, "mhdlsim_vhdl_run");
end Mhdlsim;
