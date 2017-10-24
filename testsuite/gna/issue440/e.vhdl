package p is
  generic(
    function generic_f(b:bit) return boolean
  );
end package;

entity e is end entity;
architecture a of e is
  function f(b:bit) return boolean is begin return false; end function;
  package q is new work.p generic map(f);
begin
  assert q.generic_f('0') report "msg1" severity note;
end architecture;
