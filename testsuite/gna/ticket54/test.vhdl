entity test is
end entity;

architecture a of test is
  procedure check(value : boolean) is
  begin
    assert value;
  end procedure;
begin

  main : process
    procedure do_some_other_stuff is
    begin
      check(false);
    end procedure;

    procedure do_stuff is
    begin
      check(true);
      do_some_other_stuff;
      check(true);
    end procedure;
  begin
    check(true);
    do_stuff;
    wait;
  end process;
end architecture;
