entity e5 is
end e5;

architecture behav of e5 is
  type my_rec is record
    b : boolean;
    n : work.no_pkg.no_type;
  end record;
  constant cst : my_rec := (b => true, n => (others => true));
begin
end behav;
