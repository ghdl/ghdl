package sim_types_pkg is
    type descriptor_t is record
		--address : std_ulogic_vector(dma_addr_range);
		--length : std_ulogic_vector(dma_len_range);
		address : natural;
		length : positive;
    end record;

    procedure call_report (v : natural);
end package;

package body sim_types_pkg is
  procedure call_report (v : natural) is
  begin
    report "call_report " & natural'image(v) severity note;
  end call_report;
end sim_types_pkg;

