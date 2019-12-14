use work.const_pkg.all;

configuration tester_conf of tester is
    for a
        for checker : check
            use entity work.generic_check
                generic map (
                    i => c
                );
        end for;
    end for;
end configuration;
