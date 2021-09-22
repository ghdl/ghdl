library ieee ;
    use ieee.std_logic_1164.all ;

library std ;
    use std.textio.all ;

entity match_operators is
end entity match_operators ;

architecture arch of match_operators is

    -- Index is (L,R)
    type result_table_t is array(std_ulogic, std_ulogic) of std_ulogic ;

    function "not"(x : result_table_t) return result_table_t is
        variable rv : result_table_t := (others =>(others =>'-'));
    begin
        for l in x'range(1) loop
            for r in x'range(2) loop
                rv(l, r) := not x(l,r) ;
            end loop ;
        end loop ;
        return rv ;
    end function ;

    function "or"(a, b : result_table_t) return result_table_t is
        variable rv : result_table_t := (others =>(others =>'-'));
    begin
        for l in std_ulogic loop
            for r in std_ulogic loop
                rv(l,r) := a(l,r) or b(l,r) ;
            end loop ;
        end loop ;
        return rv ;
    end function ;

    procedure print_table(name : string ; x : result_table_t) is
        variable currentline : line ;
    begin
        write(currentline, "Table: " & name) ;
        writeline(output, currentline) ;
        writeline(output, currentline) ;
        -- Header
        write(currentline, string'("L\R|")) ;
        for idx in x'range(2) loop
            write(currentline, std_ulogic'image(idx) & ' ') ;
        end loop ;
        writeline(output, currentline) ;
        write(currentline, string'("---+")) ;
        for idx in x'range(2) loop
            if idx /= std_ulogic'right then
                write(currentline, string'("----")) ;
            else
                write(currentline, string'("---")) ;
            end if ;
        end loop ;
        writeline(output, currentline) ;

        for l in x'range(1) loop
            for r in x'range(2) loop
                if r = std_ulogic'left then
                    write(currentline, std_ulogic'image(l) & '|' ) ;
                end if ;
                write(currentline, std_ulogic'image(x(l, r)) & ' ') ;
            end loop ;
            writeline(output, currentline) ;
        end loop ;
        writeline(output, currentline) ;
    end procedure ;

    constant equal_table : result_table_t := (
       -- U   X   0   1   Z   W   L   H   -
        ('U','U','U','U','U','U','U','U','1'), -- 'U'
        ('U','X','X','X','X','X','X','X','1'), -- 'X'
        ('U','X','1','0','X','X','1','0','1'), -- '0'
        ('U','X','0','1','X','X','0','1','1'), -- '1'
        ('U','X','X','X','X','X','X','X','1'), -- 'Z'
        ('U','X','X','X','X','X','X','X','1'), -- 'W'
        ('U','X','1','0','X','X','1','0','1'), -- 'L'
        ('U','X','0','1','X','X','0','1','1'), -- 'H'
        ('1','1','1','1','1','1','1','1','1')  -- '-'
    ) ;


    constant less_table : result_table_t := (
       -- U   X   0   1   Z   W   L   H   -
        ('U','U','U','U','U','U','U','U','X'), -- 'U'
        ('U','X','X','X','X','X','X','X','X'), -- 'X'
        ('U','X','0','1','X','X','0','1','X'), -- '0'
        ('U','X','0','0','X','X','0','0','X'), -- '1'
        ('U','X','X','X','X','X','X','X','X'), -- 'Z'
        ('U','X','X','X','X','X','X','X','X'), -- 'W'
        ('U','X','0','1','X','X','0','1','X'), -- 'L'
        ('U','X','0','0','X','X','0','0','X'), -- 'H'
        ('X','X','X','X','X','X','X','X','X')  -- '-'
    ) ;

    constant inequal_table          : result_table_t := not equal_table ;

    constant less_equal_table       : result_table_t := equal_table or less_table ;

    constant greater_equal_table    : result_table_t := not less_table ;

    constant greater_table          : result_table_t := not less_equal_table ;

begin

    tb : process
        variable y      : std_ulogic ;
        variable check  : std_ulogic ;
    begin
        -- Print the tables out
        print_table("?=",  equal_table) ;
        print_table("?/=", inequal_table) ;
        print_table("?<",  less_table) ;
        print_table("?<=", less_equal_table) ;
        print_table("?>=", greater_equal_table) ;
        print_table("?>",  greater_table) ;
        for l in std_ulogic loop
            for r in std_ulogic loop
                -- Match Equality
                y     := l ?=  r ;
                check := equal_table(l, r) ;
                assert(y = check) report
                    std_ulogic'image(l) & " ?=  " & std_ulogic'image(r) & " expected: " & std_ulogic'image(check) & " got: " & std_ulogic'image(y)
                    severity failure ;

                -- Match Inequality
                y     := l ?/= r ;
                check := inequal_table(l, r) ;
                assert(y = check) report
                    std_ulogic'image(l) & " ?/= " & std_ulogic'image(r) & " expected: " & std_ulogic'image(check) & " got: " & std_ulogic'image(y)
                    severity failure ;

                -- Match Less
                y     := l ?<  r ;
                check := less_table(l, r) ;
                assert(y = check) report
                    std_ulogic'image(l) & " ?<  " & std_ulogic'image(r) & " expected: " & std_ulogic'image(check) & " got: " & std_ulogic'image(y)
                    severity failure ;

                -- Match Less Equal
                y     := l ?<= r ;
                check := less_equal_table(l, r) ;
                assert(y = check) report
                    std_ulogic'image(l) & " ?<= " & std_ulogic'image(r) & " expected: " & std_ulogic'image(check) & " got: " & std_ulogic'image(y)
                    severity failure ;

                -- Match Greater Equal
                y     := l ?>= r ;
                check := greater_equal_table(l, r) ;
                assert(y = check) report
                    std_ulogic'image(l) & " ?>= " & std_ulogic'image(r) & " expected: " & std_ulogic'image(check) & " got: " & std_ulogic'image(y)
                    severity failure ;

                -- Match Greater
                y     := l ?>  r ;
                check := greater_table(l, r) ;
                assert(y = check) report
                    std_ulogic'image(l) & " ?>  " & std_ulogic'image(r) & " expected: " & std_ulogic'image(check) & " got: " & std_ulogic'image(y)
                    severity failure ;

            end loop ;
        end loop ;
        wait ;
    end process ;

end architecture arch ;
