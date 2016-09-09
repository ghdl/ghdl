--  LLVM binding
--  Copyright (C) 2014 Tristan Gingold
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
--  along with GHDL; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.
with LLVM.Core; use LLVM.Core;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Interfaces.C; use Interfaces.C;

package LLVM.BitWriter is
   -- Writes a module to an open file descriptor. Returns 0 on success.
   -- Closes the Handle. Use dup first if this is not what you want.
   function WriteBitcodeToFileHandle(M : ModuleRef; Handle : File_Descriptor)
                                    return int;

   -- Writes a module to the specified path. Returns 0 on success.
   function WriteBitcodeToFile(M : ModuleRef; Path : Cstring)
                              return int;
private
   pragma Import (C, WriteBitcodeToFileHandle, "LLVMWriteBitcodeToFileHandle");
   pragma Import (C, WriteBitcodeToFile, "LLVMWriteBitcodeToFile");
end LLVM.BitWriter;
