entity paren13 is
end paren13;

architecture behav of paren13
is
begin
  proc: process
    type string_acc is access string;
    variable hel : string_acc := new string'("hello");
    impure function a return string_acc is
    begin
      return hel;
    end a;
    function a return natural is
    begin
      return 5;
    end a;
    constant b : natural := 2;
  begin
    assert proc.a(b) = 'e';
    wait;
  end process;
end behav;
