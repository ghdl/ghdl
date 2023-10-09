entity repro2_sub is
  generic (v : natural);
end;

architecture behav of repro2_sub is
begin
  assert v = 8 severity failure;
end;

entity repro2 is
end;

architecture behav of repro2 is
  type nat_vec is array (natural range <>) of natural;

  constant fib : nat_vec(0 to 6) := (1,1,2,3,5,8,13);

  alias fib2 : natural is fib(5);
begin
  dut: entity work.repro2_sub generic map (v => fib2);
end;
