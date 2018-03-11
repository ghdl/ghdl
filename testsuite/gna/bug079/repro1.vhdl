entity repro1 is
end repro1;

architecture behav of repro1 is
begin

  process
    variable var : natural;

    procedure proc (var : natural) is
    begin
      assert var = 5;
    end;
  begin
    var := 5;
    proc (var => var);
    wait;
  end process;
end behav;
