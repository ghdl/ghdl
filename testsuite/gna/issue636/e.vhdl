entity e is
  function f(x:bit) return bit is begin return '0'; end function;
  attribute a :bit;
  constant  c :bit := f('0')'a;
end entity;
