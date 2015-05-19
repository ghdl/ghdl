entity ent is
end entity;

architecture a of ent is
  type ivec is array (integer range -2 to 2) of integer;

  function fun1 return ivec is
  begin
    return (0,1,2,3,4);
  end function;

  function fun2(arg : integer) return ivec is
  begin
    return (0,1,2,3,4);
  end function;

begin
  main : process
  begin
    report integer'image(fun1'length);
    report integer'image(fun2(2)'length);
    report integer'image(fun1'left);
    report integer'image(fun2(2)'left);
    report integer'image(fun1'right);
    report integer'image(fun2(2)'right);
    wait;
  end process;
end architecture;
