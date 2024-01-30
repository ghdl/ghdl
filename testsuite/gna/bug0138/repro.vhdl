entity repro is
end;

architecture behav of repro is
  type t_rec is record
    a, b : natural;
    c : bit;
  end record;

  constant cst : t_rec :=
    (a => 12,
     b => 15,
     d => 'a',
     c => '1');
begin
end;
