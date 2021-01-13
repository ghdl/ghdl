--  Iir to ortho translator.
--  Copyright (C) 2002 - 2014 Tristan Gingold
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

package Trans.Chap14 is
   function Translate_Array_Attribute_To_Range (Expr : Iir) return Mnode;

   --  Read signal value FIELD of signal SIG.
   function Get_Signal_Value_Field
     (Sig : O_Enode; Sig_Type : Iir; Field : O_Fnode)
         return O_Lnode;

   function Get_Signal_Field (Sig : Mnode; Field : O_Fnode) return O_Lnode;

   function Translate_Length_Array_Attribute (Expr : Iir; Rtype : Iir)
                                                 return O_Enode;
   function Translate_Low_Array_Attribute (Expr : Iir) return O_Enode;
   function Translate_High_Array_Attribute (Expr : Iir) return O_Enode;
   function Translate_Range_Array_Attribute (Expr : Iir) return O_Lnode;
   function Translate_Right_Array_Attribute (Expr : Iir) return O_Enode;
   function Translate_Left_Array_Attribute (Expr : Iir) return O_Enode;
   function Translate_Ascending_Array_Attribute (Expr : Iir) return O_Enode;

   function Translate_High_Low_Type_Attribute
     (Atype : Iir; Is_High : Boolean) return O_Enode;

   --  Return the value of the left bound/right bound/direction of scalar
   --  type ATYPE.
   function Translate_Left_Type_Attribute (Atype : Iir) return O_Enode;
   function Translate_Right_Type_Attribute (Atype : Iir) return O_Enode;
   function Translate_Dir_Type_Attribute (Atype : Iir) return O_Enode;

   function Translate_Val_Attribute (Attr : Iir) return O_Enode;
   function Translate_Pos_Attribute (Attr : Iir; Res_Type : Iir)
                                        return O_Enode;

   function Translate_Succ_Pred_Attribute (Attr : Iir) return O_Enode;

   function Translate_Image_Attribute (Attr : Iir) return O_Enode;
   function Translate_Value_Attribute (Attr : Iir) return O_Enode;

   function Translate_Event_Attribute (Attr : Iir) return O_Enode;
   function Translate_Active_Attribute (Attr : Iir) return O_Enode;
   function Translate_Last_Value_Attribute (Attr : Iir) return O_Enode;

   function Translate_Last_Time_Attribute (Prefix : Iir; Field : O_Fnode)
                                              return O_Enode;

   function Translate_Driving_Value_Attribute (Attr : Iir) return O_Enode;

   function Translate_Driving_Attribute (Attr : Iir) return O_Enode;

   function Translate_Path_Instance_Name_Attribute (Attr : Iir)
                                                       return O_Enode;
end Trans.Chap14;
