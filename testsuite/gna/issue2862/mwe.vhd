entity ent1 is
  generic (
    FOO : integer range 2 to 14 := 11
  );
end entity;

architecture structural of ent1 is
begin
  x: if FOO < 9 generate
  else generate
  end generate;
end architecture structural;
