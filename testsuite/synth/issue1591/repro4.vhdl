entity repro4 is
  port (o : out boolean);
end entity repro4;

architecture arch of repro4 is
begin
  testG : if true generate
    signal b : boolean := true;
  begin
    o <= b;
  end generate testG;
end;
