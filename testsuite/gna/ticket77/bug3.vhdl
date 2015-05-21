entity ent3 is
end entity;

architecture a of ent3 is
begin
  main : process
  begin
--    wait for 0 ns; -- Comment and it exits with code 1
    std.env.stop(7);
    wait;
  end process;
end architecture;
