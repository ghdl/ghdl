entity ent is
end entity;

architecture a of ent is
  constant SimulationTime_c  : time    := 10000 ms;
begin
  process begin
    report "Hello world" severity note;
    wait;
  end process;
end;

