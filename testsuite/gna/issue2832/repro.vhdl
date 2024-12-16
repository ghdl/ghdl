entity repro is
end entity;

architecture a of repro is
begin
  process is
  begin
    if now = 0 ns then
      wait for 0_223_372_036_854_775_807 fs;
    end if;
    report "expect overflow";
    wait for 1_000 sec;
  end process;
end architecture;
