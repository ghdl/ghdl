entity leftofrightof is
end entity;

architecture subclass_variable of leftofrightof is

begin
    process
        variable i: character := 'e';
    begin
        report "i = " & character'image(i);
        report "leftof(i) = " & character'image(character'leftof(i));
        report "rightof(i) = " & character'image(character'rightof(i));
        wait;
    end process;
end architecture;
