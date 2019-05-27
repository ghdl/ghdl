--  VHDL regeneration from internal nodes.
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
--  along with GHDL; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.

with Vhdl.Nodes; use Vhdl.Nodes;
with Vhdl.Tokens; use Vhdl.Tokens;
with PSL.Types; use PSL.Types;

package Vhdl.Disp_Vhdl is
   type Disp_Ctxt is abstract tagged null record;
   procedure Start_Hbox (Ctxt : in out Disp_Ctxt) is abstract;
   procedure Close_Hbox (Ctxt : in out Disp_Ctxt) is abstract;
   procedure Start_Vbox (Ctxt : in out Disp_Ctxt) is abstract;
   procedure Close_Vbox (Ctxt : in out Disp_Ctxt) is abstract;
   procedure Disp_Token (Ctxt : in out Disp_Ctxt; Tok : Token_Type)
      is abstract;
   procedure Start_Lit (Ctxt : in out Disp_Ctxt; Tok : Token_Type)
      is abstract;
   procedure Disp_Char (Ctxt : in out Disp_Ctxt; C : Character)
      is abstract;
   procedure Close_Lit (Ctxt : in out Disp_Ctxt)
     is abstract;

   subtype Ctxt_Class is Disp_Ctxt'Class;

   procedure Disp_Str (Ctxt : in out Ctxt_Class; Str : String);

   -- General procedure to display a node.
   -- Mainly used to dispatch to other functions according to the kind of
   -- the node.
   procedure Disp_Vhdl (Ctxt : in out Ctxt_Class; N : Iir);
   procedure Disp_Vhdl (N : Iir);

   procedure Disp_PSL_NFA (Ctxt : in out Ctxt_Class; N : PSL_NFA);
   procedure Disp_PSL_NFA (N : PSL_NFA);

   procedure Disp_Expression (Expr: Iir);
   --  Display an expression.
end Vhdl.Disp_Vhdl;
