entity repro is
end;

architecture behav of repro is
  constant nbv : bit_vector(1 downto 0) := "01";

  type arg_t is array (natural range <>) of bit_vector;
  constant null_arg_t: arg_t(0 downto 1) := (others=>null_arr_t);
begin
end;

