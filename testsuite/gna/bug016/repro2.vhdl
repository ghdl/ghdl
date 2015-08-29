configuration conf of repro is
  for behav
   for c : comp
     use entity work.comp;
     for behav
      for c2 : comp2
       use entity work.comp2 (behav);
      end for;
     end for;
   end for;
  end for;
end conf;

architecture behav of comp2 is
begin
  assert s = '1';
end behav;

