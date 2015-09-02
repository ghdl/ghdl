entity simple is
end;

architecture behav of simple is
begin
  process
  begin
    report "hello";
    assert false report "SUCESS";
  end process;
end behav;
