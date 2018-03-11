entity e is end entity;
architecture a of e is
  function f(b:integer:=0) return string is begin return "abc"; end function;
  function f               return string is begin return "def"; end function;
  subtype r is integer range 1 to 2;
begin
  assert false report "x: " & f(r) severity note;
end architecture;
