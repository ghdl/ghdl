entity mini_repro1 is
    generic(
        WIDTH_MAX: positive:= 640
    );
end entity;

architecture sim of mini_repro1 is
    -- Subtype declaration, seems to be the source of the crash
    subtype Prng_Seed_t is bit_vector(WIDTH_MAX downto 0);

    type Test_Pattern_Options_t is record
        PRNGSeed         : Prng_Seed_t;
    end record Test_Pattern_Options_t;
    
    -- Signal with a type from the generic package
    signal pattern_options : Test_Pattern_Options_t;
begin
  process
    variable v : Test_Pattern_Options_t;
  begin
    assert v = pattern_options;
    wait;
  end process;
end architecture;
