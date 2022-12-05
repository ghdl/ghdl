architecture arch of ent is
begin
  process
    --  Comment for :v:
    variable v : natural;
  begin
    while v < 10 loop
      v := v + 1;
    end loop;
    wait;
  end process;
end arch;
