entity repro is
end repro;

architecture behav of repro is
begin
  Genf: for i in 1 to 2 generate
  begin
     blk : block
     begin
     end block;
  end generate;

  geni : if true generate
  begin
     blk : block
     begin
     end block;
  end generate;
end behav;
