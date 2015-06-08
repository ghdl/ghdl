entity ent is
end entity;

architecture a of ent is
  component comp is
  end component;

  for inst : comp use configuration cfg;
begin
  inst : comp;
end architecture;

