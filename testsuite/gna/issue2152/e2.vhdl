entity e2 is end;

architecture a of e2 is
  type line is access string;
begin
    process
        variable q : line(1 to 10);
    begin
      q := new string(1 to 10);
        wait;
    end process;
end;
