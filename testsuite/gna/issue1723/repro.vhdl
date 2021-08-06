entity repro is
end;

architecture behav of repro is
     type my_vector is array (natural range <>) of integer;
   type my_vector_vector is array (natural range <>) of my_vector;

   constant C_1 : my_vector(1 downto 0) := (others => 1);
   constant C_2 : my_vector(2 downto 0) := (others => 2);

   constant C_FOO : my_vector_vector(1 downto 0) := (
      0 => C_1,
      1 => C_2
   );
begin
end;

