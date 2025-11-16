package local_pkg is
   function ternary_generic generic(type T) parameter(cond: boolean; when_true: T; when_false: T) return T;
   function ternary is new ternary_generic generic map (integer);
   function ternary is new ternary_generic generic map (boolean);
end package;

package body local_pkg is
   function ternary_generic generic(type T) parameter(cond: boolean; when_true: T; when_false: T) return T is
   begin
      if cond then
         return when_true;
      else
         return when_false;
      end if;
   end function;
end package body;

use work.local_pkg.ternary;

entity tb_ghdl is
end entity;

architecture arch of tb_ghdl is
begin
   process
   begin
      report to_string(ternary(false, 1, 2) );
      report to_string(ternary(true, true, false));
      wait;
   end process;
end architecture;
