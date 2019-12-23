--  Highler level API to build a netlist - do some optimizations.
--  Copyright (C) 2019 Tristan Gingold
--
--  This file is part of GHDL.
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; if not, write to the Free Software
--  Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston,
--  MA 02110-1301, USA.

with Netlists.Builders; use Netlists.Builders;

package Netlists.Folds is
   --  Build a const from VAL.  Result is either a Const_UB32 or a Const_Bit.
   function Build2_Const_Uns (Ctxt : Context_Acc; Val : Uns64; W : Width)
                             return Net;

   --  Build a const from VAL.  Result is either a Const_SB32 or a Const_Bit.
   function Build2_Const_Int (Ctxt : Context_Acc; Val : Int64; W : Width)
                             return Net;

   function Build2_Const_Vec (Ctxt : Context_Acc; W : Width; V : Uns32_Arr)
                             return Net;

   --  Concatenate nets of ELS in reverse order.  So if ELS(L .. R), then
   --  ELS(L) will be at offset 0.
   function Build2_Concat (Ctxt : Context_Acc; Els : Net_Array) return Net;

   --  Truncate I to width W.  Merge if the input is an extend.
   function Build2_Trunc (Ctxt : Context_Acc;
                          Id : Module_Id;
                          I : Net;
                          W : Width;
                          Loc : Location_Type)
                         return Net;

   --  Zero extend, noop or truncate I so that its width is W.
   function Build2_Uresize (Ctxt : Context_Acc;
                            I : Net;
                            W : Width;
                            Loc : Location_Type := No_Location)
                           return Net;

   --  Sign extend, noop or truncate I so that its width is W.
   function Build2_Sresize (Ctxt : Context_Acc;
                            I : Net;
                            W : Width;
                            Loc : Location_Type := No_Location)
                           return Net;

   --  Same as Build_Extract, but return I iff extract all the bits.
   function Build2_Extract
     (Ctxt : Context_Acc; I : Net; Off, W : Width) return Net;
end Netlists.Folds;
