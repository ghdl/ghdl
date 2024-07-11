entity a is
  generic (s : string);
end entity a;

architecture rtl of a is
begin
end architecture rtl;

entity test is
end entity test;

architecture sim of test is
  signal b : boolean;
begin
  -- bug
  dut_0 : entity work.a
  generic map (s => dut_0'instance_name);
  -- bug
  dut_1 : entity work.a
  generic map (s => dut_1'path_name);
  -- works
  dut_2 : entity work.a
  generic map (s => dut_2'simple_name);
  -- bug
  dut_3 : entity work.a
  generic map (s => dut_2'instance_name);
end architecture sim;

