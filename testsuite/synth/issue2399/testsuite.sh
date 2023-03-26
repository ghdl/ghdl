#! /bin/sh

. ../../testenv.sh

synth_only repro

exit 0

synth --std=08                                                                 \
  --work=fpga_cores third_party/fpga_cores/src/common_pkg.vhd                  \
                    third_party/fpga_cores/src/interface_types_pkg.vhd         \
                    third_party/fpga_cores/src/synchronizer.vhd                \
                    third_party/fpga_cores/src/axi_pkg.vhd                     \
                    third_party/fpga_cores/src/axi_stream_debug.vhd            \
                    third_party/fpga_cores/src/axi_stream_frame_slicer.vhd     \
                    third_party/fpga_cores/src/axi_stream_frame_padder.vhd     \
                    third_party/fpga_cores/src/sr_delay.vhd                    \
                    third_party/fpga_cores/src/skidbuffer.vhd                  \
                    third_party/fpga_cores/src/ram_inference_dport.vhd         \
                    third_party/fpga_cores/src/ram_inference.vhd               \
                    third_party/fpga_cores/src/rom_inference.vhd               \
                    third_party/fpga_cores/src/pipeline_context_ram.vhd        \
                    third_party/fpga_cores/src/edge_detector.vhd               \
                    third_party/fpga_cores/src/pulse_sync.vhd                  \
                    third_party/fpga_cores/src/axi_stream_delay.vhd            \
                    third_party/fpga_cores/src/sync_fifo.vhd                   \
                    third_party/fpga_cores/src/axi_stream_credit.vhd           \
                    third_party/fpga_cores/src/axi_stream_ram.vhd              \
                    third_party/fpga_cores/src/axi_stream_master_adapter.vhd   \
                    third_party/fpga_cores/src/axi_stream_flow_control.vhd     \
                    third_party/fpga_cores/src/axi_stream_replicate.vhd        \
                    third_party/fpga_cores/src/axi_stream_fifo.vhd             \
                    third_party/fpga_cores/src/axi_stream_frame_fifo.vhd       \
                    third_party/fpga_cores/src/async_fifo.vhd                  \
                    third_party/fpga_cores/src/axi_stream_width_converter.vhd  \
                    third_party/fpga_cores/src/axi_stream_mux.vhd              \
                    third_party/fpga_cores/src/axi_stream_demux.vhd            \
                    third_party/fpga_cores/src/axi_stream_arbiter.vhd          \
  --work=work       third_party/airhdl/dvbs2_encoder_regs_pkg.vhd              \
                    third_party/airhdl/dvbs2_encoder_regs.vhd                  \
                    third_party/bch_generated/bch_128x64.vhd                   \
                    third_party/bch_generated/bch_192x64.vhd                   \
                    third_party/bch_generated/bch_128x32.vhd                   \
                    third_party/bch_generated/bch_160x16.vhd                   \
                    third_party/bch_generated/bch_128x16.vhd                   \
                    third_party/bch_generated/bch_168x8.vhd                    \
                    third_party/bch_generated/bch_192x32.vhd                   \
                    third_party/bch_generated/bch_192x8.vhd                    \
                    third_party/bch_generated/bch_128x8.vhd                    \
                    third_party/bch_generated/bch_160x32.vhd                   \
                    third_party/bch_generated/bch_160x64.vhd                   \
                    third_party/bch_generated/bch_192x16.vhd                   \
                    third_party/bch_generated/bch_160x8.vhd                    \
                    rtl/axi_baseband_scrambler.vhd                             \
                    rtl/axi_bit_interleaver.vhd                                \
                    rtl/axi_constellation_mapper.vhd                           \
                    rtl/axi_physical_layer_framer.vhd                          \
                    rtl/axi_physical_layer_scrambler.vhd                       \
                    rtl/axi_bbframe_length_enforcer.vhd                        \
                    rtl/dvb_utils_pkg.vhd                                      \
                    rtl/constellation_mapper_pkg.vhd                           \
                    rtl/bch_encoder_mux.vhd                                    \
                    rtl/axi_ldpc_table.vhd                                     \
                    rtl/ldpc/ldpc_input_sync.vhd                               \
                    rtl/axi_ldpc_encoder_core.vhd                              \
                    rtl/axi_ldpc_encoder.vhd                                   \
                    rtl/axi_plframe_header.vhd                                 \
                    rtl/dummy_frame_generator.vhd                              \
                    rtl/plframe_header_pkg.vhd                                 \
                    rtl/ldpc/ldpc_pkg.vhd                                      \
                    rtl/ldpc/ldpc_tables_pkg.vhd                               \
                    rtl/dvbs2_encoder.vhd                                      \
                    rtl/axi_bch_encoder.vhd                                    \
  -e dvbs2_encoder

echo "Test successful"
