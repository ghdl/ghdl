--  Although being part of GRT, the readline binding should be independent of
--  it (for easier reuse).

with System; use System;

package Grt.Readline is
   subtype Fat_String is String (Positive);
   type Char_Ptr is access Fat_String;
   pragma Convention (C, Char_Ptr);
   --  A C string (which is NUL terminated) is represented as a (thin) access
   --  to a fat string (a string whose range is 1 .. integer'Last).
   --  The use of an access to a constrained array allows a representation
   --  compatible with C.  Indexing of object of that type is safe only for
   --  indexes until the NUL character.

   function Readline (Prompt : Char_Ptr) return Char_Ptr;
   function Readline (Prompt : Address) return Char_Ptr;
   pragma Import (C, Readline);

   procedure Free (Buf : Char_Ptr);
   pragma Import (C, Free);

   procedure Add_History (Line : Char_Ptr);
   pragma Import (C, Add_History);

   function Strlen (Str : Char_Ptr) return Natural;
   pragma Import (C, Strlen);

   pragma Linker_Options ("-lreadline");
end Grt.Readline;
