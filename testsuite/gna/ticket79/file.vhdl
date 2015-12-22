entity sub_ent is
end entity;

architecture a of sub_ent is
  signal sig : integer := 0;
begin
  sig <= 1 after 1 ns;
end architecture;

entity ent is
end entity;

architecture a of ent is
  signal sig : integer;
begin
  dut : entity work.sub_ent;
  sig <= << signal dut.sig : integer >>;

  monitor : process
  begin
    wait on sig;
    report to_string(sig);
  end process;
end architecture;
