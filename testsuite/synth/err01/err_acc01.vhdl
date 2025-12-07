entity err_acc01 is
end;

architecture behav of err_acc01 is
begin
  process
    type line is access string;
    variable v : line;
  begin
    assert v.all = "";
    wait;
  end process;
end behav;
