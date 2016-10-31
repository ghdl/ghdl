entity bug is
    port(data: out integer);
end entity bug;

architecture arc of bug is
begin
end architecture arc;

entity bug_sim is
end entity bug_sim;

architecture sim of bug_sim is
    signal data: natural;
begin
    u0: entity work.bug
        port map(data => data);
end architecture sim;
