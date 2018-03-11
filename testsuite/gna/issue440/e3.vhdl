package p3 is
 generic(
   -- function generic_f(b:bit) return boolean
   function f(b:bit) return boolean
   );
 constant c : boolean := f('0');
end package;

entity e3 is end entity;
architecture a of e3 is
 function f(b:bit) return boolean is begin return false; end function;
 package q is new work.p3 generic map(f);
begin
 assert q.c = false report "bad value" severity failure;
end architecture;
