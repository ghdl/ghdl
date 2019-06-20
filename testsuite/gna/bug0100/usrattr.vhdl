entity usrattr is
end usrattr;

architecture behav of usrattr is
  type rec is record
      data : natural;
  end record;
  signal myrec : rec;
  signal s : boolean;
begin
  s <= myrec.data'attr;
end behav;
