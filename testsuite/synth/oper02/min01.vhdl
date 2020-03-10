entity min01 is
  port (a, b : natural;
        o : out natural);
end min01;

architecture behav of min01 is
begin
  o <= minimum (a, b);
end behav;
