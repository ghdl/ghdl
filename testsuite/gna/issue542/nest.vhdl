entity nest is
  constant c : natural := 1;
end nest;

architecture behav of nest is
  --  Not valid in vhdl-02.
  signal c : boolean;
begin
end behav;
