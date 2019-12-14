entity nochoice2 is
end;

architecture behav of nochoice2 is
  constant n : string (1 to 2) := "ab";
begin
  process
  begin
    case n is
      when "aa" => null;
      when "bbb" => null;
    end case;
  end process;
end behav;
