entity e is end;

architecture a of e is
begin
    process
        variable v : real;

        subtype t is real range 2.0 to 2.0;
        variable v1 : t;

        variable v2 : t := 2.0;

        constant c1 : t := 2.0;
    begin
        report "exp: -1.7976931348623157e308, act: " & real'image(v);                   
        report "exp: ???,                     act: " & real'image(v * 1000.0);          
        assert v = real'left severity failure;         -- Should not trigger. Does not trigger
                                                
        report "exp: 2.0,                     act: " & real'image(v1);                  
        report "exp: 1.002e3,                 act: " & real'image(v1 + 1000.0);         
        assert v1 = t'left severity failure;           -- Should not trigger. Does not trigger

        report "exp: 2.0,                     act: " & real'image(v2);                  
        report "exp: 1.002e3,                 act: " & real'image(v2 + 1000.0);         
        assert v2 = t'left severity failure;           -- Should not trigger. Does not trigger

        report "exp: 2.0,                     act: " & real'image(c1);                  
        report "exp: 1.002e3,                 act: " & real'image(c1 + 1000.0);         
        assert c1 = t'left severity failure;           -- Should not trigger. Does not trigger

        std.env.finish;                                                                                       
    end process;
end;
