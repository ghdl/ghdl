--  GHDL Run Time (GRT) -  'image subprograms.
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
with Grt.Types; use Grt.Types;
with Grt.Rtis; use Grt.Rtis;

package Grt.Images is
   procedure Ghdl_Image_B2
     (Res : Std_String_Ptr; Val : Ghdl_B2; Rti : Ghdl_Rti_Access);
   procedure Ghdl_Image_E8
     (Res : Std_String_Ptr; Val : Ghdl_E8; Rti : Ghdl_Rti_Access);
   procedure Ghdl_Image_E32
     (Res : Std_String_Ptr; Val : Ghdl_E32; Rti : Ghdl_Rti_Access);
   procedure Ghdl_Image_I32 (Res : Std_String_Ptr; Val : Ghdl_I32);
   procedure Ghdl_Image_F64 (Res : Std_String_Ptr; Val : Ghdl_F64);
   procedure Ghdl_Image_P64
     (Res : Std_String_Ptr; Val : Ghdl_I64; Rti : Ghdl_Rti_Access);
   procedure Ghdl_Image_P32
     (Res : Std_String_Ptr; Val : Ghdl_I32; Rti : Ghdl_Rti_Access);
private
   pragma Export (Ada, Ghdl_Image_B2, "__ghdl_image_b2");
   pragma Export (C, Ghdl_Image_E8, "__ghdl_image_e8");
   pragma Export (C, Ghdl_Image_E32, "__ghdl_image_e32");
   pragma Export (C, Ghdl_Image_I32, "__ghdl_image_i32");
   pragma Export (C, Ghdl_Image_F64, "__ghdl_image_f64");
   pragma Export (C, Ghdl_Image_P64, "__ghdl_image_p64");
   pragma Export (C, Ghdl_Image_P32, "__ghdl_image_p32");
end Grt.Images;
