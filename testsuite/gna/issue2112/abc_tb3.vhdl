entity abc_tb3 is
end;

architecture sim of abc_tb3 is
  type Indices_t is array (natural range <>) of bit_vector;

  constant c : Indices_t := (0 => bit_vector'(x"00"),
                             1 => bit_vector'(x"01"));
begin
end architecture sim;
