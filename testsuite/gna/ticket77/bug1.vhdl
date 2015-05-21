entity ent1 is
end entity;

architecture a of ent1 is
begin
  main : process
  begin
    wait for 0 ns; -- Comment and it exits with code 1
    std.env.stop(1);
    wait;
  end process;
end architecture;
