entity leftof03 is
end;

architecture behav of leftof03 is
begin
  process
    variable vi : integer;
    subtype t_ddigit is natural range 10 downto 1;
    variable vd : t_ddigit;
  begin
    vd := 10;
    assert t_ddigit'leftof(vd) = 4;
    wait;
  end process;
end behav;
