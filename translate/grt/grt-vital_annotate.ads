--  GHDL Run Time (GRT) - VITAL annotator.
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
with Grt.Sdf; use Grt.Sdf;

package Grt.Vital_Annotate is
   pragma Elaborate_Body (Grt.Vital_Annotate);

   procedure Sdf_Header (Context : Sdf_Context_Type);
   procedure Sdf_Celltype (Context : Sdf_Context_Type);
   procedure Sdf_Instance (Context : in out Sdf_Context_Type;
                           Instance : String;
                           Status : out Boolean);
   procedure Sdf_Instance_End (Context : Sdf_Context_Type;
                               Status : out Boolean);
   procedure Sdf_Generic (Context : in out Sdf_Context_Type;
                          Name : String;
                          Ok : out Boolean);

   procedure Register;
end Grt.Vital_Annotate;
