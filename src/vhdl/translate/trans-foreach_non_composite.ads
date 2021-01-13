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

--  Handle a composite type TARG/TARG_TYPE and apply DO_NON_COMPOSITE
--  on each non composite type.
--  There is a generic parameter DATA which may be updated
--    before indexing an array by UPDATE_DATA_ARRAY.
generic
   type Data_Type is private;
   type Composite_Data_Type is private;
   with procedure Do_Non_Composite (Targ      : Mnode;
                                    Targ_Type : Iir;
                                    Data      : Data_Type);

   --  This function should extract the base of DATA.
   with function Prepare_Data_Array (Targ      : Mnode;
                                     Targ_Type : Iir;
                                     Data      : Data_Type)
                                    return Composite_Data_Type;

   --  This function should index DATA.
   with function Update_Data_Array (Data      : Composite_Data_Type;
                                    Targ_Type : Iir;
                                    Index     : O_Dnode)
                                   return Data_Type;

   --  This function is called at the end of a record process.
   with procedure Finish_Data_Array (Data : in out Composite_Data_Type)
      is null;

   --  This function should stabilize DATA.
   with function Prepare_Data_Record (Targ      : Mnode;
                                      Targ_Type : Iir;
                                      Data      : Data_Type)
                                     return Composite_Data_Type;

   --  This function should extract field EL of DATA.
   with function Update_Data_Record (Data      : Composite_Data_Type;
                                     Targ_Type : Iir;
                                     El        : Iir_Element_Declaration)
                                    return Data_Type;

   --  This function is called at the end of a record process.
   with procedure Finish_Data_Record (Data : in out Composite_Data_Type)
     is null;

procedure Trans.Foreach_Non_Composite (Targ      : Mnode;
                                       Targ_Type : Iir;
                                       Data      : Data_Type);
