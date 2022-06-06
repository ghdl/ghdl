entity full_adder_tb is
end entity full_adder_tb;

architecture sim of full_adder_tb is
begin

  process
  begin
    stx.env(i).b;
  wait;
  end process;
end architecture sim;
