entity e is end entity;
architecture a of e is
  function f(b:integer:=0) return string is begin return "abc"; end function;
  function f               return string is begin return "def"; end function;
begin
  assert false report "x: " & f(1 to 2) severity note;
end architecture;
