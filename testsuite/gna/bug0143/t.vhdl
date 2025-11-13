entity t is
  generic (s : string := "");
end t;

architecture arch of t is
  constant v : real := real'value(s);
begin
end;
 
