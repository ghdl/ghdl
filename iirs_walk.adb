--  Walk in iirs nodes.
--  Copyright (C) 2009 Tristan Gingold
--
--  GHDL is free software; you can redistribute it and/or modify it under
--  the terms of the GNU General Public License as published by the Free
--  Software Foundation; either version 2, or (at your option) any later
--  version.
--
--  GHDL is distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
--  for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with GCC; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.

package body Iirs_Walk is
   function Walk_Chain (Chain : Iir; Cb : Walk_Cb) return Walk_Status
   is
      El : Iir;
      Status : Walk_Status := Walk_Continue;
   begin
      El := Chain;
      while El /= Null_Iir loop
         Status := Cb.all (El);
         exit when Status /= Walk_Continue;
         El := Get_Chain (El);
      end loop;
      return Status;
   end Walk_Chain;

   function Walk_Sequential_Stmt (Stmt : Iir; Cb : Walk_Cb) return Walk_Status;


   function Walk_Sequential_Stmt_Chain (Chain : Iir; Cb : Walk_Cb)
                                       return Walk_Status
   is
      El : Iir;
      Status : Walk_Status := Walk_Continue;
   begin
      El := Chain;
      while El /= Null_Iir loop
         Status := Cb.all (El);
         exit when Status /= Walk_Continue;
         Status := Walk_Sequential_Stmt (El, Cb);
         exit when Status /= Walk_Continue;
         El := Get_Chain (El);
      end loop;
      return Status;
   end Walk_Sequential_Stmt_Chain;

   function Walk_Sequential_Stmt (Stmt : Iir; Cb : Walk_Cb) return Walk_Status
   is
      Status : Walk_Status := Walk_Continue;
      Chain : Iir;
   begin
      case Iir_Kinds_Sequential_Statement (Get_Kind (Stmt)) is
         when Iir_Kind_Signal_Assignment_Statement
           | Iir_Kind_Null_Statement
           | Iir_Kind_Assertion_Statement
           | Iir_Kind_Report_Statement
           | Iir_Kind_Wait_Statement
           | Iir_Kind_Return_Statement
           | Iir_Kind_Procedure_Call_Statement
           | Iir_Kind_Next_Statement
           | Iir_Kind_Exit_Statement
           | Iir_Kind_Variable_Assignment_Statement =>
            null;
         when Iir_Kind_For_Loop_Statement
           | Iir_Kind_While_Loop_Statement =>
            Status := Walk_Sequential_Stmt_Chain
              (Get_Sequential_Statement_Chain (Stmt), Cb);
         when Iir_Kind_Case_Statement =>
            Chain := Get_Case_Statement_Alternative_Chain (Stmt);
            while Chain /= Null_Iir loop
               Status := Walk_Sequential_Stmt_Chain
                 (Get_Associated (Chain), Cb);
               exit when Status /= Walk_Continue;
               Chain := Get_Chain (Chain);
            end loop;
         when Iir_Kind_If_Statement =>
            Chain := Stmt;
            while Chain /= Null_Iir loop
               Status := Walk_Sequential_Stmt_Chain
                 (Get_Sequential_Statement_Chain (Chain), Cb);
               exit when Status /= Walk_Continue;
               Chain := Get_Else_Clause (Chain);
            end loop;
      end case;
      return Status;
   end Walk_Sequential_Stmt;

   function Walk_Assignment_Target (Target : Iir; Cb : Walk_Cb)
                                   return Walk_Status
   is
      Chain : Iir;
      Status : Walk_Status := Walk_Continue;
   begin
      case Get_Kind (Target) is
         when Iir_Kind_Aggregate =>
            Chain := Get_Association_Choices_Chain (Target);
            while Chain /= Null_Iir loop
               Status := Walk_Assignment_Target (Get_Associated (Chain), Cb);
               exit when Status /= Walk_Continue;
               Chain := Get_Chain (Chain);
            end loop;
         when others =>
            Status := Cb.all (Target);
      end case;
      return Status;
   end Walk_Assignment_Target;
end Iirs_Walk;
