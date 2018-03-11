entity e is end entity;
architecture h of e is
  type r1 is record a :integer; end record;
  type r2 is record b :integer; end record;
  type r3 is record a :r1     ; end record;
  type r4 is record a :r2     ; end record;
  function f(a :integer := 1) return r3 is begin return (a=>(a=>a)); end function;
  function f(a :integer := 2) return r4 is begin return (a=>(b=>a)); end function;
  constant c1 :integer := f.a.a;
  constant c2 :integer := f.a.b;
  constant c3 :integer := f(3).a.a;
  constant c4 :integer := f(4).a.b;
begin
  assert false report integer'image(c1) severity note;
  assert false report integer'image(c2) severity note;
  assert false report integer'image(c3) severity note;
  assert false report integer'image(c4) severity note;
end architecture;
