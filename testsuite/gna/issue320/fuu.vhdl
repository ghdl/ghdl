entity fuu is
end entity;

architecture fum of fuu is
    function fie  return string is
    begin
        return "11010011"; 
    end function;
begin
    process
    begin
        report "expression'right = " & integer'image(fie'RIGHT);
        wait;
    end process;
end architecture;
