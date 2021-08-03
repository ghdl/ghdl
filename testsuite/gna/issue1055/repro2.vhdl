entity repro2 is
end;

architecture behav of repro2 is
  constant nbv : bit_vector(1 downto 0) := "01";

  type arg_t is array (natural range <>) of bit_vector;
  constant null_arg_t: arg_t(1 downto 0) := (others=>nbv);
begin
end;

