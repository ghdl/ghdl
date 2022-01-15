entity gen_delay is
  generic (type T);

  port (i : in T;
        o : out T);
end gen_delay;

architecture arch of gen_delay is
begin
  o <= i;
end arch;

entity dly2 is
  port (i : in bit_vector(7 downto 0);
        o : out bit_vector(7 downto 0));
end dly2;

architecture struct of dly2 is
begin
  inst: entity work.gen_delay
    generic map (T => bit_vector(7 downto 0))
    port map (i => i, o => o);
end struct;
