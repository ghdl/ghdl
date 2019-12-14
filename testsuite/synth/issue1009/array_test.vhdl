entity array_test is
  port (
    o : out integer
  );
end array_test;

architecture rtl of array_test is
  type t_array is array (natural range <>) of integer;
  constant rec_constant : t_array := (1 => 27.777 us / 83.333 ns);
  constant rec_constant2 : t_array := rec_constant;
begin
  o <= rec_constant2 (1);
end rtl;
