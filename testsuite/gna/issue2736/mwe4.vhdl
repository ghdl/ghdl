package mwe_pkg is
    procedure drive_sig_default (signal s : out bit);
end package;

package body mwe_pkg is
    procedure drive_sig_default (signal s : out bit) is
    begin
        s <= '1';
    end procedure;
end package body;

--------------------------------------------------------------------------------

-- use work.mwe_pkg.all;

entity mwe_generic is
    generic (
        procedure drive_sig (signal s : out bit) is drive_sig_default
    );
end entity;

architecture bhv of mwe_generic is
    signal a : bit;
begin
    -- Case 1 does not work
    -- drive_sig(a);

    -- Case 2 works
    process is
    begin
        drive_sig(a);
        wait;
    end process;

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
  inst : entity work.mwe_generic;
end architecture;

