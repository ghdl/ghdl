entity comp1 is
  port (
    a_i  : in bit_vector(3 downto 0)
  );
end entity;

architecture arch of comp1 is
begin
end arch;

entity mwe is
end entity;

architecture arch of mwe is
  signal clk : bit := '0';
  signal a   : bit_vector(3 downto 0);
begin
  process
  begin
    wait until clk'stable;
  end process;

  x_comp1 : entity work.comp1
    port map (a_i  => a or a);
end arch;
