package cfuncs is
    procedure hello_world;
    attribute foreign of hello_world : procedure is "VHPIDIRECT hello_world";
end package cfuncs;
package body cfuncs is
    procedure hello_world is
    begin report "VHPIDIRECT hello_world" severity failure; end;
end package body cfuncs;
