entity repro_bit_oper is
  port (x : in bit; y : out boolean);
end;

architecture a of repro_bit_oper is
begin
  y <= true when x else false;
end;
