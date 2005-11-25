with Grt.Types; use Grt.Types;

package Grt.Arch_None is
   function Get_Time_Stamp return Ghdl_U64;
   pragma Inline (Get_Time_Stamp);
end Grt.Arch_None;
