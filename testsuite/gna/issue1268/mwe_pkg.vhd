package mwe_pkg is
    type mwe_t is protected
        procedure send_random_data (
            constant num_bits : integer range 1 to 32 := 1
        );

    end protected;

end package;

package body mwe_pkg is
    type mwe_t is protected body
        procedure send_random_data (
            constant num_bits : integer range 1 to 32 := 1
        ) is
            variable rnd : boolean;
        begin
            -- init random seed
          report rnd'instance_name;
        end procedure;
    end protected body;

end package body;
