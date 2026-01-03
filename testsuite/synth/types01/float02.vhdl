entity float02 is
  port (a : out bit);
end;

architecture behav of float02 is
  function err (s : string := "") return real is
  begin
    return real'value(s);
  end err;
  subtype my_real is real range err to 50.0;
begin
  a <= '1' when my_real'low < 40.0;
end behav;
