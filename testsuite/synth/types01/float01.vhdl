entity float01 is
  port (a : out bit);
end;

architecture behav of float01 is
  function err (s : string := "") return real is
  begin
    return real'value(s);
  end err;
  subtype my_real is real range 50.0 to err;
begin
  a <= '1' when my_real'high > 60.0;
end behav;
