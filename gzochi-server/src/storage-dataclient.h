#ifndef GZOCHID_STORAGE_DATACLIENT_H
#define GZOCHID_STORAGE_DATACLIENT_H

#include "gzochid-storage.h"

extern gzochid_storage_engine_interface
gzochid_storage_engine_interface_dataclient;

void gzochid_dataclient_storage_context_set_dataclient
(gzochid_storage_context *, GzochidDataClient *);

void gzochid_dataclient_storage_release_lock (gzochid_storage_store *,
					      GBytes *);

#endif /* GZOCHID_STORAGE_DATACLIENT_H */
