entity st4 is
end;

architecture arch of st4 is
  constant v : string (1 to 5) := "hello";
  subtype mypos is natural range v'subtype'range;
  subtype my2 is mypos range 2 to 6;
begin
end arch;
