entity uattr4 is
end;

architecture behav of uattr4 is
begin
  --  Not possible.
  assert false report "ent4'ent_name = " & work.ent4'ent_name severity note;
end behav;
