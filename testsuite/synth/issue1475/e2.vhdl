entity e2 is
end;

architecture behav of e2 is
  type my_type is (lit1, lit2);

  function "+" (l : my_type; r : natural) return my_type is
  begin
    return lit2;
  end "+";

  signal s : my_type := lit1;
begin
  assert s + (-1) = lit2;
end behav;
