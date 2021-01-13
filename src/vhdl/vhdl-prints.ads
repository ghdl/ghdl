--  VHDL regeneration from internal nodes.
--  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
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

with Vhdl.Nodes; use Vhdl.Nodes;
with Vhdl.Tokens; use Vhdl.Tokens;
with PSL.Types; use PSL.Types;

package Vhdl.Prints is
   --  Vertical alignment
   type Valign_Type is
     (
      --  Align the colon (which separates identifier from mode or subtype).
      Valign_Colon,

      --  Align the assign token (either for declarations or assignments).
      Valign_Assign,

      --  Align the subtype indication.
      Valign_Typemark
     );

   type Disp_Ctxt is abstract tagged null record;
   procedure Start_Hbox (Ctxt : in out Disp_Ctxt) is abstract;
   procedure Close_Hbox (Ctxt : in out Disp_Ctxt) is abstract;
   procedure Start_Vbox (Ctxt : in out Disp_Ctxt) is abstract;
   procedure Close_Vbox (Ctxt : in out Disp_Ctxt) is abstract;
   procedure Valign (Ctxt : in out Disp_Ctxt; Point : Valign_Type)
     is abstract;
   procedure Disp_Token (Ctxt : in out Disp_Ctxt; Tok : Token_Type)
      is abstract;
   procedure Start_Lit (Ctxt : in out Disp_Ctxt; Tok : Token_Type)
      is abstract;
   procedure Disp_Char (Ctxt : in out Disp_Ctxt; C : Character)
      is abstract;
   procedure Close_Lit (Ctxt : in out Disp_Ctxt)
     is abstract;

   subtype Ctxt_Class is Disp_Ctxt'Class;

   --  Helper that calls Disp_Char for every character of STR.
   procedure Disp_Str (Ctxt : in out Ctxt_Class; Str : String);

   --  Return True if a space should be displayed between PREV_TOK and TOK.
   function Need_Space (Tok, Prev_Tok : Token_Type) return Boolean;

   -- General procedure to display a node.
   -- Mainly used to dispatch to other functions according to the kind of
   -- the node.
   procedure Disp_Vhdl (Ctxt : in out Ctxt_Class; N : Iir);
   procedure Disp_Vhdl (N : Iir);

   procedure Disp_PSL_NFA (Ctxt : in out Ctxt_Class; N : PSL_NFA);
   procedure Disp_PSL_NFA (N : PSL_NFA);

   procedure Disp_Expression (Expr: Iir);
   --  Display an expression.
end Vhdl.Prints;
