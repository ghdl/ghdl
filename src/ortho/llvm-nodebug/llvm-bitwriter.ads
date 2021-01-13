--  LLVM binding
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
