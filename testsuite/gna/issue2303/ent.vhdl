entity ent is
end entity;

architecture  a of ent is
  type type_t is range 0 to 8;
  constant foo : type_t := 3 ** 2;
begin
end architecture;
