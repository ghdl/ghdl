package local_pkg_proc is
   procedure ternary_generic generic(type T) parameter(cond: boolean; when_true: T; when_false: T; res : out T);
   procedure ternary is new ternary_generic generic map (integer);
   procedure ternary is new ternary_generic generic map (boolean);
end package;

package body local_pkg_proc is
   procedure ternary_generic generic(type T) parameter(cond: boolean; when_true: T; when_false: T; res : out T) is
   begin
      if cond then
        res := when_true;
      else
        res := when_false;
      end if;
   end;
end package body;

use work.local_pkg_proc.ternary;

entity tb_proc is
end entity;

architecture arch of tb_proc is
begin
   process
     variable vint : integer;
     variable vbool : boolean;
   begin
      ternary(false, 1, 2, vint);
      report integer'image(vint);
      ternary(true, true, false, vbool);
      report boolean'image(vbool);
      wait;
   end process;
end architecture;
