entity paren6 is
end paren6;

architecture behav of paren6
is
begin
  process
    type string_acc is access string;
    variable a : string_acc := new string'("hello");
    constant b : natural := 3;
  begin
    assert a(b) = 'l';
    wait;
  end process;
end behav;
