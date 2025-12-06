entity rightof02 is
end;

architecture behav of rightof02 is
begin
  process
    variable vi : integer;
    subtype t_ddigit is natural range 10 downto 1;
    variable vd : t_ddigit;
  begin
    vi := 4;
    assert natural'rightof(vi) = 5;

    vd := 3;
    assert t_ddigit'rightof(vd) = 2;
    wait;
  end process;
end behav;
