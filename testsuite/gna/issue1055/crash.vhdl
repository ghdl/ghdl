package crash_pkg is

  type vec_t is array (natural range <>) of real;
  constant null_vec_t : vec_t(0 downto 1) := (others=>0.0);

  type arr_t is array (natural range <>, natural range <>) of real;
  constant null_arr_t : arr_t(0 downto 1, 0 downto 1) := (others=>(others=>(0.0)));

  type arg_t is array (natural range <>) of arr_t;
  constant null_arg_t: arg_t(0 downto 1) := (others=>null_arr_t);

end crash_pkg;
