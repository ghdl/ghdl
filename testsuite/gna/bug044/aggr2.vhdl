entity aggr2 is
end;

architecture behav of aggr2 is
  constant g : natural := 1;
begin
  process
    variable v : bit_vector(3 downto 0);
  begin
     v (g + 2 downto g) := (2 downto 0 => '1');
     wait;
  end process;
end;
