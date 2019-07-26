entity econcat2_87 is
end econcat2_87;

architecture behav of econcat2_87 is
  constant c1 : string (21 downto 17) := "hello";
  constant c2 : string (6 downto 1) := " world";
  constant r : string := "" & c1 & "" & c2;
begin
  process
  begin
    case True is
      when c1 & c2 = "hello world" => null;
      when false => null;
    end case;

    assert r'left = 21 severity failure;
    assert r'right = 11 severity failure;
    wait;
  end process;
end;
