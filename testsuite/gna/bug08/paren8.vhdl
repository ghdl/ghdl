entity paren8 is
end paren8;

architecture behav of paren8
is
begin
  process
    function a return string is
    begin
      return "hello";
    end a;
    constant b : natural := 5;
  begin
    assert a(b) = 'o';
    wait;
  end process;
end behav;
