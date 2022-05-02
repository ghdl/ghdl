entity oper1 is
  port (
    a:  bit;
    err : out bit);
end;

architecture behav of oper1 is
  signal b_err : bit_vector(7 downto 0);
begin
  err <= '1' when b_err /= (b_err'range => inp(1));
end behav;
