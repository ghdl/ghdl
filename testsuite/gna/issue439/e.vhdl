package p is
  generic(
    function generic_f(b:bit) return boolean;
    function generic_f(b:boolean) return bit
  );
  function f(b:bit    ) return boolean;
  function f(b:boolean) return bit    ;
end package;

package body p is
  function f(b:bit    ) return boolean is begin return generic_f(b); end function;
  function f(b:boolean) return bit     is begin return generic_f(b); end function;
end package body;

entity e is end entity;
architecture a of e is
  function f(b:bit    ) return boolean is begin return false; end function;
  function f(b:boolean) return bit     is begin return  '0' ; end function;
  package q is new work.p generic map(f,f);
begin
  assert q.f('0') report "msg2" severity note;
end architecture;
