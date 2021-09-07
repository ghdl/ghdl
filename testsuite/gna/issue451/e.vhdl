entity e is end entity;
architecture h of e is
  type a is array(natural range<>, natural range<>) of integer_vector;
  function f(x, y:integer := 0) return a is begin return (0 to 3=>(0 to 1=>(0 to 6=>y))); end function;
  -- If the association list was mandatory f()(2,0)(5) would be far less confusing.
  constant c :integer := f(2,0)(5);

  type b is array(natural range<>) of integer_vector;
  function g(x:integer := 0) return b is begin return (0 to 3=>(0 to 6=>45)); end function;
  -- If the association list was mandatory g()(2)(5) would be far less confusing.
  constant d :integer := g(2)(5);
begin
  assert false report integer'image(c) severity note;
  assert false report integer'image(d) severity note;
end architecture;
