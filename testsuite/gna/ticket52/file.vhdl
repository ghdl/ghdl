entity ent is
  port (
    prt : out integer);
end entity;

architecture a of ent is
  signal sig : integer;
begin
  prt <= 1;
  sig <= prt;
end architecture;

