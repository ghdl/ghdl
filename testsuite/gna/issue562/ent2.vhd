entity ent2 is
end entity;

architecture a of ent2 is
  constant max : natural := 2;
  signal p : natural range 1 to max;
begin

  inst : entity work.ent
    generic map (max => max)
    port map (p => p);
end;
