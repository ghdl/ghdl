entity a2 is
  generic (s : string);
end entity  ;

architecture rtl of a2 is
begin
  assert s = ":test2(sim):dut_0:" report s severity failure;
end architecture rtl;

entity test2 is
end entity test2;

architecture sim of test2 is
  signal b : boolean;
begin
  dut_0 : entity work.a2
  generic map (s => dut_0'instance_name);
end architecture sim;

