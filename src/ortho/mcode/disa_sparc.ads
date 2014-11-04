with System;

package Disa_Sparc is
   --  Call-back used to find a relocation symbol.
   type Symbol_Proc_Type is access procedure (Addr : System.Address;
                                              Line : in out String;
                                              Line_Len : in out Natural);

   --  Disassemble instruction at ADDR, and put the result in LINE/LINE_LEN.
   procedure Disassemble_Insn (Addr : System.Address;
                               Line : in out String;
                               Line_Len : out Natural;
                               Insn_Len : out Natural;
                               Proc_Cb : Symbol_Proc_Type);
end Disa_Sparc;
