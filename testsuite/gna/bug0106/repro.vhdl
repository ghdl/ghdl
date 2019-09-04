entity repro is
end repro;

architecture behav of repro is
  signal v : natural;
  constant c : integer := 5;
begin
  v <= natural (c)'a;
end behav;
