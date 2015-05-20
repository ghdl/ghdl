entity ent is
end entity;

architecture a of ent is
  function fun(s : string) return integer is
  begin
    return 0;
  end;
begin
  main : process
  begin
    assert fun(s(4 to 15) => "Hello world!") = 0;
    wait;
  end process;
end architecture;
