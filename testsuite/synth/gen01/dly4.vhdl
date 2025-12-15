entity gen_delay is
  generic (type T);

  port (i : in T;
        o : out T);
end gen_delay;

architecture arch of gen_delay is
begin
  o <= i;
end arch;

entity dly4 is
  port (i : in bit;
        o : out bit);
end dly4;

architecture struct of dly4 is
  component gen_delay is
    generic (type T);
    port (i : in T; o : out T);
  end component;
begin
  inst: gen_delay
    generic map (T => bit)
    port map (i => i, o => o);
end struct;
