entity paren10 is
end paren10;

architecture behav of paren10
is
begin
  process
    type string_acc is access string;
    function a return string_acc is
    begin
      return new string'("hello");
    end a;
    constant b : natural := 2;
  begin
    assert a(b) = 'e';
    wait;
  end process;
end behav;
