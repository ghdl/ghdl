entity e is end entity;
architecture h of e is
  type p is protected
    type t is range 0 to 2;
    function m return integer;
  end protected;
  type p is protected body
    function m return integer is begin return 123; end function;
  end protected body;
  shared variable v :p;
begin
  assert false report integer'image(v.m) severity note;
end architecture;
