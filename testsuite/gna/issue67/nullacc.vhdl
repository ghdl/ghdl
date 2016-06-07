entity nullacc is
end nullacc;

architecture behav of nullacc is
begin
  process
    type int_acc is access integer;
    variable v : int_acc;
  begin
    v := new integer'(7);
    assert v.all = 7 severity failure;
    deallocate (v);
    assert v.all = 0 severity note; -- access error
    wait;
  end process;
end behav;

    
