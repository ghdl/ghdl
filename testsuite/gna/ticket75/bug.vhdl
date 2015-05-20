entity ent is
end entity;

architecture a of ent is
begin
  main : process
  begin
    assert bit'value("'0'") = '0'; -- Works
    assert bit'value("'1'") = '1'; -- Works
    assert bit'value("0") = '0'; -- Fails
    assert bit'value("1") = '1'; -- Fails
    wait;
  end process;
end architecture;
