entity repro is
end repro;

architecture behav of repro is
  type int_vector is array (natural range <>) of integer;

  constant c1 : int_vector (0 to 1) := 12 & 13;
  constant c2 : int_vector (0 to 1) := 14 & 15;
  constant p : boolean := c1 = c2;
  constant p1 : boolean := c1 < c2;
begin
  process
  begin
    case true is
      when p => null;
      when true => null;
    end case;
    wait;
  end process;
end behav;

