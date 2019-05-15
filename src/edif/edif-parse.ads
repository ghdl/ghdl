with Edif.Nodes; use Edif.Nodes;

package Edif.Parse is
   --  Simple parser: return generic constructs (lists).
   --  Do not try to interpret EDIF.
   function Parse_File_Simple return Node;

   --  Parse as EDIF 2.0.0
   --  There is almost no error recovery: the parser stops at the first error,
   --  and it return Null_Node.
   function Parse_Edif200 return Node;
end Edif.Parse;
