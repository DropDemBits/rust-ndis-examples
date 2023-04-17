#include "wrapper.h"

void wdf_driver_config_init(
 PWDF_DRIVER_CONFIG        Config,
 PFN_WDF_DRIVER_DEVICE_ADD EvtDriverDeviceAdd
) {
  WDF_DRIVER_CONFIG_INIT(Config, EvtDriverDeviceAdd);
}

NTSTATUS wdf_driver_create(
    PDRIVER_OBJECT         DriverObject,
    PCUNICODE_STRING       RegistryPath,
    PWDF_OBJECT_ATTRIBUTES DriverAttributes,
    PWDF_DRIVER_CONFIG     DriverConfig,
   WDFDRIVER              *Driver
) {
 return WdfDriverCreate(DriverObject, RegistryPath, DriverAttributes, DriverConfig, Driver); 
}
