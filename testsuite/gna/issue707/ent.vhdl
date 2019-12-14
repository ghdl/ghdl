entity ent is
end entity;

architecture a of ent is
  subtype my_low_rng is integer range 0 to 1;
  subtype my_high_rng is integer range 2 to 3;

--  constant my_good_booleans : boolean_vector(0 to 3) :=
--    (0 to 1 => true, 2 to 3 => false);
  constant my_bad_booleans : boolean_vector(0 to 3) :=
    (my_low_rng => true, my_high_rng => false);
begin

  process begin
    report "Hello world" severity note;
    wait;
  end process;

end;
