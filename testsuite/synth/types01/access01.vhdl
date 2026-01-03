entity access01 is
  port (a : out bit);
end;

architecture behav of access01 is
  type my_line is access string(1 to 80);
begin
  a <= '1';
end behav;
