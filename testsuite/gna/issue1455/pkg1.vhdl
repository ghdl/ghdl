package pkg1 is
  type my_arr2D_t is array (natural range <>) of real_vector;

  constant my_arr2D: my_arr2D_t := (
    (0.0, 0.1),
    (1.0, 1.1)
  );
end;
