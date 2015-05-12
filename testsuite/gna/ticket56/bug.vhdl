entity test is
end entity;

architecture a of test is
  function fun(var : boolean) return boolean is
  begin
    return var;
  end function;
begin
  main : process
    constant c : boolean := fun;
  begin
    wait;
  end process;
end architecture;
