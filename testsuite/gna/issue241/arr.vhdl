entity arr is
end;

architecture behav of arr is
  type arr_type is array (natural range <>) of natural;
  constant a : arr_type (2 downto 1) := (1 | 2 => 3);
  constant b : boolean := a (1) = a (2);
begin
  process
  begin
    case true is
      when b => null;
      when false => null;
    end case;
    wait;
  end process;
end behav;
