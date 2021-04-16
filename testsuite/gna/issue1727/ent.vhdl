entity ent is
end entity;

architecture beh of ent is
type t_type is (NAME1, NAME2);
begin
  process
    constant c_string : string := "something";
  begin
    report "Length is " & to_string(c_string'lenght);
    report "Length is " & to_string(t_type'image(NAME1)'lenght);
    wait;
  end process;
end architecture beh;
