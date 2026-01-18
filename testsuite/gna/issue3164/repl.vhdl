entity unit is
    generic (type t);
    port (i: in t);
end entity;

architecture behavioral of unit is
begin
    process is
    begin
        report "Hello!";
        wait;
    end process;
end architecture;

entity top is
end entity;

-- WORKING:
-- architecture behavioral of top is
--     signal s : boolean;
-- begin
--     inst: entity work.unit generic map (boolean) port map (s);
-- end architecture;

-- BROKEN (trans-chap4.adb:43 access check failed):
architecture behavioral of top is
    component unit is
        generic (type t);
        port (i: in t);
    end component;
    signal s : boolean;
begin
    inst: entity work.unit generic map (boolean) port map (s);
end architecture;

-- BROKEN, I think... (type of signal interface not compatible with type of port):
-- architecture behavioral of top is
--     component unit is
--         generic (type t);
--         port (i: in t);
--     end component;
--     signal s : boolean;
-- begin
--     inst: unit generic map (boolean) port map (s);
-- end architecture;
