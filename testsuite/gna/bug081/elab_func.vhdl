package elab_func is
  function f return natural;
end elab_func;

package body elab_func is
  constant c : natural := f;
  function f return natural is
  begin
    return 5;
  end f;
end elab_func;
