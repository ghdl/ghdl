package body Mutils is
   function Clog2 (V : Uns64) return Integer
   is
      Low : Natural;
   begin
      if V >= 2**16 then
         if V >= 2**32 then
            Low := 32;
         else
            Low := 16;
         end if;
      else
         if V >= 2**8 then
            Low := 8;
         else
            Low := 0;
         end if;
      end if;
      for I in Low .. 63 loop
         if 2**I >= V then
            return I;
         end if;
      end loop;
      return 64;
   end Clog2;

   function Is_Power2 (V : Uns64) return Boolean is
   begin
      return (V and (V - 1)) = 0;
   end Is_Power2;
end Mutils;
