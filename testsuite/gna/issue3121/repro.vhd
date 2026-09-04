--  An external name that reads a signal declared inside an instance.
--  Here the process that reads it appears *before* the instantiation.

entity inner is
  port (o : out bit);
end entity;

architecture a of inner is
  signal sig : bit := '1';
begin
  o <= sig;
end architecture;

entity repro is
  port (q : out bit);
end entity;

architecture a of repro is
  signal s : bit;
  signal r : bit;
begin
  p : process (all)
    alias x is << signal inst.sig : bit >>;
  begin
    r <= x;
  end process;

  inst : entity work.inner port map (o => s);

  q <= r and s;
end architecture;
