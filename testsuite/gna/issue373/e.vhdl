entity x is
  port(
    t :out bit_vector(0 to 0);
    z :out bit_vector(0 to 0)
  );
end entity;
architecture a of x is begin end architecture;

entity e is end entity;
architecture a of e is
  constant z :integer := 0;
  subtype t is bit_vector(0 to 0);
  signal actual_for_t :bit;
  signal actual_for_z :t;
begin
  inst: entity work.x port map(
    t(z) => actual_for_t,
    t(z) => actual_for_z
  );
end architecture;
