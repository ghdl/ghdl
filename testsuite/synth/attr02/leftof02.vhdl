entity leftof02 is
end;

architecture behav of leftof02 is
begin
  process
    variable vi : integer;
    subtype t_ddigit is natural range 10 downto 1;
    variable vd : t_ddigit;
  begin
    vi := 4;
    assert natural'leftof(vi) = 3;

    vd := 3;
    assert t_ddigit'leftof(vd) = 4;
    wait;
  end process;
end behav;
