entity econcat1 is
end econcat1;

architecture behav of econcat1 is
  constant c1 : string (1 to 5) := "hello";
  constant c2 : string (6 downto 1) := " world";
  constant r : string := c1 & c2;
begin
  process
  begin
    case True is
      when c1 & c2 = "hello world" => null;
      when false => null;
    end case;

    assert r'left = 1 severity failure;
    assert r'right = 11 severity failure;
    wait;
  end process;
end;
