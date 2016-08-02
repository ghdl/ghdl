entity leftofrightof is
end entity;

architecture subclass_constant of leftofrightof is

begin

    process
        constant i: integer := 1;
    begin
        report "constant i = " & integer'image(i);
        report "integer'leftof(i) = " & integer'image(integer'leftof(i));
        wait;
    end process;
    process
        constant j: integer := 1;
    begin
        report "constant j = " & integer'image(j);
        report "integer'rightof(j) = " & integer'image(integer'rightof(j));
        wait;
    end process;
end architecture;
