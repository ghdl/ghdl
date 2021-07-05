entity repro1 is
  generic (type t);
end repro1;

architecture behav of repro1 is
begin
end behav;

entity repro is
  generic (type t);
end repro;

architecture behav of repro is
begin
  inst: entity work.repro1
    generic map (t => t);
end behav;
