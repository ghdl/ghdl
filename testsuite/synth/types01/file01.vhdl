entity file01 is
  port (a : out bit);
end;

architecture behav of file01 is
  type mtext is file of string;
  subtype my_text is mtext;
begin
  a <= '1';
end behav;
