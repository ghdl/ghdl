entity leftofrightof is
end entity;

architecture subclass_variable of leftofrightof is

begin
    process
        variable i: integer := 1;
    begin
        report "variable i = " & integer'image(i);
        report "integer'leftof(i) = " & integer'image(integer'leftof(i));
        wait;
    end process;
    process
        variable j: integer := 1;
    begin
        report "variable j = " & integer'image(j);
        report "integer'rightof(j) = " & integer'image(integer'rightof(j));
        wait;
    end process;
end architecture;
