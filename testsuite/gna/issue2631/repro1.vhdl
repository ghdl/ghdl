entity top is
  generic ( COUNTER_BITS : NATURAL := 7);
end entity;

architecture impl of top is
  type T_ARRAY is array (NATURAL range <>) of BIT_VECTOR;
  signal counter_int : T_ARRAY(0 to 5)(COUNTER_BITS-1 downto 0);
  signal other_ctrs : T_ARRAY(0 to 4)(COUNTER_BITS-1 downto 0);
begin
  other_ctrs <= counter_int(0 to 2) & counter_int(4 to counter_int'high);
end architecture;
