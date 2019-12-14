entity econcat2 is
end econcat2;

architecture behav of econcat2 is
  constant c1 : string (1 to 5) := "hello";
  constant c2 : string (6 downto 1) := " world";
  constant r : string := c1 & c2;
begin
  process
  begin
    case True is
      when "&" (c1, c2) = "hello world" => null;
      when false => null;
    end case;
    wait;
  end process;
end;
