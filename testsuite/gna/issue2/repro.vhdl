entity repro is
    port (
        foo: in boolean
    );
end entity;

architecture foo of repro is
    signal foo_int: boolean;
begin
FUMBLE: 
    entity work.repro
        port map (
            foo => foo_int
        );
end architecture;
