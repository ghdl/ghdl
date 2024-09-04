package mwe_pkg is
    generic (
        type user_type
    );

    type storage_t is protected
        procedure add_entry (entry : user_type);
    end protected;
end package;

package body mwe_pkg is
    type storage_t is protected body
        variable user : user_type;

        procedure add_entry (entry : user_type) is
        begin
            user := entry;
        end procedure;
    end protected body;
end package body;

--------------------------------------------------------------------------------

entity mwe is
end entity;

architecture rtl of mwe is
begin
    process is
        -- here we pass bit_vector as a generic type, without any constraints
        package bv_mwe_pkg is new work.mwe_pkg generic map (bit_vector);
        variable storage : bv_mwe_pkg.storage_t;
        -- here we use a constrained bit_vector
        variable element : bit_vector(7 downto 0);
    begin
        -- of course, there is a type mismatch
        storage.add_entry(element);

        std.env.finish;
        wait;
    end process;
end architecture;
