entity sub is end entity sub;
architecture a of sub is
  signal s : integer := 0;
begin
end architecture a;

entity extsig is end entity extsig;
architecture a of extsig is
begin
  h0 : entity work.sub;
  process begin
    << signal h0.s : integer >> <= 7;
    wait;
  end process;
end architecture a;
