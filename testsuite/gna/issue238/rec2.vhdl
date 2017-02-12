entity rec2 is
end;

architecture behav of rec2 is
  type rec1 is record
    a : natural;
  end record;

  type rec1_arr is array (natural range <>) of rec1;

  function resolve (a : rec1_arr) return rec1 is
  begin
    return (a => 0);
  end resolve;

  subtype srec1 is resolve rec1;
begin
  process
    variable a : srec1;
  begin
    a.a := 5;
    wait;
  end process;
end behav;
