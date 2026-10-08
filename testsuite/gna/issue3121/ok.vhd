--  Same design, but the instantiation appears before the process that
--  reads into it through an external name.

entity ok is
  port (q : out bit);
end entity;

architecture a of ok is
  signal s : bit;
  signal r : bit;
begin
  inst : entity work.inner port map (o => s);

  p : process (all)
    alias x is << signal inst.sig : bit >>;
  begin
    r <= x;
  end process;

  q <= r and s;
end architecture;
