entity paren11 is
end paren11;

architecture behav of paren11
is
begin
  process
    type string_acc is access string;
    variable hel : string_acc := new string'("hello");
    impure function a return string_acc is
    begin
      return hel;
    end a;
    constant b : natural := 2;
  begin
    assert a(b) = 'e';
    wait;
  end process;
end behav;
