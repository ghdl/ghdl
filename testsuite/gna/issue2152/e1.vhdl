entity e1 is end;

architecture a of e1 is
  type line is access string;
begin
    process
        variable q : line(1 to 10) := new string(1 to 10);
    begin
        wait;
    end process;
end;
