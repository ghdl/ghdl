with Types; use Types;

package Mutils is
   --  Return the ceiling log2 of V.
   --  Returns 0 for 0.
   function Clog2 (V : Uns64) return Integer;
end Mutils;
