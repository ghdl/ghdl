entity e is end entity;
architecture a of e is
  type enu is (one, two);
  function one return enu is begin
    return two;
  end function;
begin
end architecture;
