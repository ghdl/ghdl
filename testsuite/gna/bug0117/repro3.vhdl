entity repro3 is
end;

architecture behav of repro3 is
  type rec1 is record
    wr : bit;
    dat : bit_vector(7 downto 0);
  end record;

  signal s : rec1;
begin
  s <= ('0', x"01");
end behav;
