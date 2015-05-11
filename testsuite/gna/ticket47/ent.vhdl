entity ent is
  generic (str : string);
end entity;

architecture a of ent is
begin
  main : process
  begin
    report str;
    wait;
  end process;
end architecture;

