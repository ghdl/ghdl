entity ent is
  generic (
    max : natural);
  port (
    p : out natural range 1 to max := 3);
end entity;

architecture a of ent is
begin
end;
