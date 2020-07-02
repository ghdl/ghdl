entity repro5 is
  port (s : out bit);
end;

architecture behav of repro5 is
  constant width : integer := 1;
  constant zeros : bit_vector(0 to width-1) := (0 to width-1 => '0');
begin
  s <= zeros(0);
end;
