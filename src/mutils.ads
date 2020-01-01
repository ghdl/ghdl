with Types; use Types;

package Mutils is
   --  Return the ceiling log2 of V.
   --  Returns 0 for 0.
   function Clog2 (V : Uns64) return Integer;

   --  Return True IFF V is 0 or a power of 2.
   function Is_Power2 (V : Uns64) return Boolean;
end Mutils;
