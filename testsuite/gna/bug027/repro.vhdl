use work.pkg.all;
use work.all;

entity repro is
end repro;

architecture behav of repro is
  component comp is
  end component;
begin
  c : comp;
end behav;
