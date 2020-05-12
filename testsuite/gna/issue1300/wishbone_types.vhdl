package wishbone_types is
    type wb_slave_out_t is record
        dat   : bit_vector;
        ack   : bit;
        stall : bit;
    end record;

    -- Common subtypes
    constant wb_cpu_data_bits : integer := 64;
    subtype wb_cpu_in_t  is wb_slave_out_t(dat(wb_cpu_data_bits-1 downto 0));

    -- GHDL: Works
--    subtype wishbone_slave_out  is wb_slave_out_t(dat(wb_cpu_data_bits-1 downto 0));

    -- GHDL: Breaks
    subtype wishbone_slave_out  is wb_cpu_in_t;
end package wishbone_types;
