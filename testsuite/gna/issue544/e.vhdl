entity e is end entity;
architecture a of e is
  function f return boolean is begin
    return false;
  end function;
begin
  assert f report "message" severity note;
end architecture;
