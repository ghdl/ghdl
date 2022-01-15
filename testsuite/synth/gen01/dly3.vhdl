entity gen_delay is
  generic (type T);

  port (i : in T;
        o : out T);
end gen_delay;

architecture arch of gen_delay is
begin
  o <= i;
end arch;

entity dly3 is
  port (i : in bit_vector(7 downto 0);
        o : out bit_vector(7 downto 0));
end dly3;

architecture struct of dly3 is
  subtype t_vect is bit_vector(7 downto 0);
begin
  inst: entity work.gen_delay
    generic map (T => t_vect)
    port map (i => i, o => o);
end struct;
