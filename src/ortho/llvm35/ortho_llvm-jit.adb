--  LLVM back-end for ortho.
--  Copyright (C) 2014 Tristan Gingold
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 2 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <gnu.org/licenses>.

package body Ortho_LLVM.Jit is
   --  procedure AddExternalFunction (Name : Cstring; Val : Address);
   --  pragma Import (C, AddExternalFunction, "ortho_AddExternalFunction");

   function GetPointerToFunction (EE : ExecutionEngineRef; Func : ValueRef)
                                 return Address;
   pragma Import (C, GetPointerToFunction, "LLVMGetPointerToFunction");

   --  Set address of non-defined global variables or functions.
   procedure Set_Address (Decl : O_Dnode; Addr : Address) is
   begin
      case Decl.Kind is
         when ON_Var_Decl | ON_Const_Decl =>
            AddGlobalMapping (Engine, Decl.LLVM, Addr);
         when ON_Subprg_Decl =>
            null;
            --  AddExternalFunction (GetValueName (Decl.LLVM), Addr);
         when others =>
            raise Program_Error;
      end case;
   end Set_Address;

   --  Get address of a global.
   function Get_Address (Decl : O_Dnode) return Address
   is
   begin
      case Decl.Kind is
         when ON_Var_Decl | ON_Const_Decl =>
            return GetPointerToGlobal (Engine, Decl.LLVM);
         when ON_Subprg_Decl =>
            return GetPointerToFunction (Engine, Decl.LLVM);
         when others =>
            raise Program_Error;
      end case;
   end Get_Address;

end Ortho_LLVM.Jit;
