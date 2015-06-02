package pkg is
  function f (a : integer) return integer;
end pkg;

package body pkg is
  function f (a : integer) return integer is
  begin
   return 1;
  end f;

  function f (a : integer) return integer is
  begin
   return 1;
  end f;
end pkg;
