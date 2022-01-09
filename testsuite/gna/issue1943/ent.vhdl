entity some_entity is
end entity;

package some_generic_package is
    generic (
        some_param : natural
    );

    component some_component is
    end component;
end package;

package some_package_instance is new work.some_generic_package
    generic map(
        8
    );

use work.some_package_instance.all;

architecture some_architecture of some_entity is
begin
end architecture;
