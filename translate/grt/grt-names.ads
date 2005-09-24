--  GHDL Run Time (GRT) -  'name* subprograms.
--  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
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
with System; use System;
with Grt.Types; use Grt.Types;
with Grt.Rtis; use Grt.Rtis;

package Grt.Names is
   procedure Ghdl_Get_Path_Name (Res : Std_String_Ptr;
                                 Ctxt : Ghdl_Rti_Access;
                                 Base : Address;
                                 Name : Ghdl_Str_Len_Ptr);

   procedure Ghdl_Get_Instance_Name (Res : Std_String_Ptr;
                                     Ctxt : Ghdl_Rti_Access;
                                     Base : Address;
                                     Name : Ghdl_Str_Len_Ptr);
private
   pragma Export (C, Ghdl_Get_Path_Name, "__ghdl_get_path_name");
   pragma Export (C, Ghdl_Get_Instance_Name, "__ghdl_get_instance_name");
end Grt.Names;
