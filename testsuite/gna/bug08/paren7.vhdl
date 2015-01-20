entity paren7 is
end paren7;

architecture behav of paren7
is
begin
  process
    type string_acc is access string;
    variable a : string_acc := new string'("hello");
    subtype b is natural range 2 to 4;
  begin
    assert a(b) = "ell";
    wait;
  end process;
end behav;
