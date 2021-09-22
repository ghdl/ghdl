entity ent is
  generic ( mygeneric : string );
end;

architecture arch of ent is
begin
  assert mygeneric = "Hello" report "Generic mygeneric is not 'Hello'!" severity failure;
end;
