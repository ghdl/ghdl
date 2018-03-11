package p2 is
 generic(
   -- function generic_f(b:bit) return boolean
   function f(b:bit) return boolean
 );
 alias generic_f is f [bit return boolean];  -- non object alias
end package;

entity e2 is end entity;
architecture a of e2 is
 function f(b:bit) return boolean is begin return false; end function;
 package q is new work.p2 generic map(f);
begin
  assert q.generic_f('0') report "OK" severity note;
  assert q.generic_f('0') = false report "Bad value" severity failure;
end architecture;
