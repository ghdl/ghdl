entity ent4 is
end entity;

architecture a of ent4 is
begin
  main : process
  begin
--    wait for 0 ns; -- Comment and it exits with code 1
    std.env.stop(0);
    wait;
  end process;
end architecture;
