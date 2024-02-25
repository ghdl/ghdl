entity err1 is
end;

architecture behav of err1 is
  type MyEnum is (LitA, LitB);
begin
  process
    variable v : MyEnum;
  begin
    case v is
      when LitA =>
        null;
    end case;
    wait;
  end process;
end behav;
