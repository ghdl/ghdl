entity rec is
end;

architecture behav of rec is
  type rec_type is record
    a, b : natural;
  end record;

  constant r1 : rec_type := (a | b => 2);
  constant b : boolean := r1.a = r1.b;
begin
  process
    variable a : integer := 5;
  begin
    case a = 5 is
      when b => null;
      when false => null;
    end case;
    wait;
  end process;
end behav;
