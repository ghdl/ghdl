entity paren9 is
end paren9;

architecture behav of paren9
is
begin
  process
    function a return string is
    begin
      return "hello";
    end a;
    subtype b is natural range 1 to 2;
  begin
    assert a(b) = "he";
    wait;
  end process;
end behav;
