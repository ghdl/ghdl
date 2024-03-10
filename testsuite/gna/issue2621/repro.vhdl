entity repro is
end;

architecture behav of repro is
begin
  process
  begin
    report "start...";
    wait for 10 ns;
    report "...end";
    report "failure" severity failure;
    wait;
  end process;
end behav;
