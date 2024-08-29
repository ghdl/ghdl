entity mwe_generic is
    generic (
        procedure drive_sig (signal s : out bit)
    );
end entity;

architecture bhv of mwe_generic is
    signal a : bit;
begin
    -- Case 1 does not work
    drive_sig(a);

    -- Case 2 works
    -- process is
    -- begin
    --     drive_sig(a);
    --     wait;
    -- end process;

end architecture;

--------------------------------------------------------------------------------

entity mwe is
end entity;

architecture bhv of mwe is
    procedure p (signal s : out bit) is
    begin
        s <= '1';
    end procedure;
begin
    inst : entity work.mwe_generic generic map (drive_sig => p);
end architecture;
