entity bug3 is
end;

architecture behavior of bug3 is
  constant c : string := "hello";
begin
    c(1 downto 0);
end behavior;
