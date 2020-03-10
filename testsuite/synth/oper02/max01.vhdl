entity max01 is
  port (a, b : natural;
        o : out natural);
end max01;

architecture behav of max01 is
begin
  o <= maximum (a, b);
end behav;
