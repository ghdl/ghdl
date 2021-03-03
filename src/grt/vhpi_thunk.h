/* GHDL Run Time (GRT) - VHPI thunks
   Copyright (C) 2021 Marlon James

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <gnu.org/licenses>.

   As a special exception, if other files instantiate generics from this
   unit, or you link this unit with other files to produce an executable,
   this unit does not by itself cause the resulting executable to be
   covered by the GNU General Public License. This exception does not
   however invalidate any other reasons why the executable file might be
   covered by the GNU Public License.
*/

/* VHPI thunk is a GHDL structure used to interface between DLL/DSO and
   GRT.  This is in particular needed on Windows where undefined references
   in DLL are not supported. Follows VPI thunks */

#include "vhpi_user.h"

typedef struct {
  int         (*vhpi_vassert) (vhpiSeverityT severity, char *formatmsg,
                               va_list args);
  vhpiHandleT (*vhpi_register_cb) (vhpiCbDataT *cb_data_p, int32_t flags);
  int         (*vhpi_remove_cb) (vhpiHandleT cb_obj);
  int         (*vhpi_disable_cb) (vhpiHandleT cb_obj);
  int         (*vhpi_enable_cb) (vhpiHandleT cb_obj);
  int         (*vhpi_get_cb_info) (vhpiHandleT object, vhpiCbDataT *cb_data_p);
  int         (*vhpi_sens_first) (vhpiValueT *sens);
  int         (*vhpi_sens_zero) (vhpiValueT *sens);
  int         (*vhpi_sens_clr) (int obj, vhpiValueT *sens);
  int         (*vhpi_sens_set) (int obj, vhpiValueT *sens);
  int         (*vhpi_sens_isset) (int obj, vhpiValueT *sens);
  vhpiHandleT (*vhpi_handle_by_name) (const char *name, vhpiHandleT scope);
  vhpiHandleT (*vhpi_handle_by_index) (vhpiOneToManyT itRel,
                                       vhpiHandleT parent, int32_t indx);
  vhpiHandleT (*vhpi_handle) (vhpiOneToOneT type, vhpiHandleT referenceHandle);
  vhpiHandleT (*vhpi_iterator) (vhpiOneToManyT type,
                                vhpiHandleT referenceHandle);
  vhpiHandleT (*vhpi_scan) (vhpiHandleT iterator);
  vhpiIntT    (*vhpi_get) (vhpiIntPropertyT property, vhpiHandleT object);
  const vhpiCharT * (*vhpi_get_str) (vhpiStrPropertyT property,
                                     vhpiHandleT object);
  vhpiRealT   (*vhpi_get_real) (vhpiRealPropertyT property,
                                vhpiHandleT object);
  vhpiPhysT   (*vhpi_get_phys) (vhpiPhysPropertyT property,
                                vhpiHandleT object);
  int         (*vhpi_protected_call) (vhpiHandleT varHdl, vhpiUserFctT userFct,
                                      void *userData);
  int         (*vhpi_get_value) (vhpiHandleT expr, vhpiValueT *value_p);
  int         (*vhpi_put_value) (vhpiHandleT object, vhpiValueT *value_p,
                                 vhpiPutValueModeT mode);
  int         (*vhpi_schedule_transaction) (vhpiHandleT drivHdl,
                                            vhpiValueT *value_p,
                                            uint32_t numValues,
                                            vhpiTimeT *delayp,
                                            vhpiDelayModeT delayMode,
                                            vhpiTimeT *pulseRejp);
  int         (*vhpi_format_value) (const vhpiValueT *in_value_p,
                                    vhpiValueT *out_value_p);
  void        (*vhpi_get_time) (vhpiTimeT *time_p, long *cycles);
  int         (*vhpi_get_next_time) (vhpiTimeT *time_p);
  int         (*vhpi_control_internal) (int command, int status);
  int         (*vhpi_vprintf) (const char *format, va_list args);
  int         (*vhpi_is_printable) ( char ch );
  int         (*vhpi_compare_handles) (vhpiHandleT handle1,
                                       vhpiHandleT handle2);
  int         (*vhpi_check_error) (vhpiErrorInfoT *error_info_p);
  int         (*vhpi_release_handle) (vhpiHandleT object);
  vhpiHandleT (*vhpi_create) (vhpiClassKindT kind, vhpiHandleT handle1,
                              vhpiHandleT handle2);
  vhpiHandleT (*vhpi_register_foreignf) (vhpiForeignDataT *foreignDatap);
  int         (*vhpi_get_foreignf_info) (vhpiHandleT hdl,
                                         vhpiForeignDataT *foreignDatap);
  size_t      (*vhpi_get_data) (int32_t id, void *dataLoc, size_t numBytes);
  size_t      (*vhpi_put_data) (int32_t id, void *dataLoc, size_t numBytes);
} vhpi_thunk;
