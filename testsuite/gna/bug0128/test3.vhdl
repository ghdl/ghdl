use std.textio.all;

entity test2 is
end entity;

architecture beh of test2 is
    
    procedure fun1(
        variable x: inout line
    ) is
    begin
    
    end procedure;
    
    procedure fun2(
        variable x: in string
    ) is
    begin
        report x severity note;
    end procedure;
            
begin
    
    process
        variable x :line;
    begin
        --doing something with x... or leave null....
        fun1(x);
        
        --How to check if x is null so that i can put an if statement around this?
        fun2(x.all);
    
    end process;

end architecture;
