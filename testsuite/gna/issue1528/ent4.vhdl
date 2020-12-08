use work.attrs_pkg.all;

entity ent4 is
  generic (name : string := "ent4 name");
  attribute ent_name of ent4: entity is name;
end entity;

architecture rtl of ent4 is
begin
  process
  begin
    report "I am: " & ent4'ent_name;
    wait;
  end process;
end rtl;
