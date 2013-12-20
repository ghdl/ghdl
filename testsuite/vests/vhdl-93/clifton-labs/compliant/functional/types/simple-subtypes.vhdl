entity test is
end test;

architecture only of test is

begin  -- only
  doit: process
    subtype tboolean        is boolean        range FALSE to TRUE;
    subtype tbit            is bit            range '0'   to '1';   
    subtype tcharacter      is character      range 'A'   to 'Z';
    subtype tseverity_level is severity_level range NOTE  to ERROR;
    subtype tinteger        is integer        range 1111  to 2222;
    subtype treal           is real           range 1.11  to 2.22;
    subtype ttime           is time           range 1 ns  to 1 hr;
    subtype tnatural        is natural        range 100   to 200;
    subtype tpositive       is positive       range 1000  to 2000;

    variable k1  : tboolean;
    variable k2  : tbit;
    variable k3  : tcharacter;
    variable k4  : tseverity_level;
    variable k5  : tinteger;
    variable k6  : treal;
    variable k7  : ttime;
    variable k8  : tnatural;
    variable k9  : tpositive;
    
  begin  -- process doit
    assert( k1 = tboolean'left        ) report "TEST FAILED" severity failure;
    assert( k2 = tbit'left            ) report "TEST FAILED" severity FAILURE;
    assert( k3 = tcharacter'left      ) report "TEST FAILED" severity FAILURE;
    assert( k4 = tseverity_level'left ) report "TEST FAILED" severity FAILURE;
    assert( k5 = tinteger'left        ) report "TEST FAILED" severity FAILURE;
    assert( k6 = treal'left           ) report "TEST FAILED" severity FAILURE;
    assert( k7 = ttime'left           ) report "TEST FAILED" severity FAILURE;
    assert( k8 = tnatural'left        ) report "TEST FAILED" severity FAILURE;
    assert( k9 = tpositive'left       ) report "TEST FAILED" severity FAILURE;
    report "TEST PASSED";
    wait;
  end process doit;
end only;
