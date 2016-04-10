/* gzochid-storage.h: Public SPI for gzochid storage engine system
 * Copyright (C) 2015 Julian Graham
 *
 * gzochi is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef GZOCHID_STORAGE_H
#define GZOCHID_STORAGE_H

#include <glib.h>
#include <gmodule.h>
#include <gmp.h>
#include <sys/time.h>

#define GZOCHID_STORAGE_EFAILURE -1
#define GZOCHID_STORAGE_ENOTFOUND -2
#define GZOCHID_STORAGE_ETXFAILURE -3

#define GZOCHID_STORAGE_CREATE 0x01
#define GZOCHID_STORAGE_EXCL 0x02

struct _gzochid_storage_context
{
  gpointer environment;
};

typedef struct _gzochid_storage_context  gzochid_storage_context;

struct _gzochid_storage_store
{
  gzochid_storage_context *context;
  gpointer database;
};

typedef struct _gzochid_storage_store gzochid_storage_store;

struct _gzochid_storage_transaction
{
  gzochid_storage_context *context;
  gpointer txn;
  gboolean rollback;
  gboolean should_retry;
};

typedef struct _gzochid_storage_transaction gzochid_storage_transaction;

/* The interface provided by storage engine modules. */

struct _gzochid_storage_engine_interface
{
  char *name;

  gzochid_storage_context *(*initialize) (char *);

  /* Close the specified storage context. */

  void (*close_context) (gzochid_storage_context *);

  /* Destroy the storage context rooted at the specified path by removing all
     associated files, including the path itself. */

  void (*destroy_context) (char *);
  gzochid_storage_store *(*open) 
    (gzochid_storage_context *, char *, unsigned int);
  void (*close_store) (gzochid_storage_store *);

  /* Destroy the store rooted at the specified storage context and path by 
     removing all associated files. */

  void (*destroy_store) (gzochid_storage_context *, char *);

  gzochid_storage_transaction *(*transaction_begin) (gzochid_storage_context *);
  gzochid_storage_transaction *(*transaction_begin_timed)
    (gzochid_storage_context *, struct timeval);
  void (*transaction_commit) (gzochid_storage_transaction *);
  void (*transaction_rollback) (gzochid_storage_transaction *);
  void (*transaction_prepare) (gzochid_storage_transaction *);
  char *(*transaction_get) 
    (gzochid_storage_transaction *, gzochid_storage_store *, char *, size_t, 
     size_t *);
  char *(*transaction_get_for_update)
    (gzochid_storage_transaction *, gzochid_storage_store *, char *, size_t, 
     size_t *);
  void (*transaction_put)
    (gzochid_storage_transaction *, gzochid_storage_store *, char *, size_t, 
     char *, size_t);
  int (*transaction_delete)
    (gzochid_storage_transaction *, gzochid_storage_store *, char *, size_t);
  char *(*transaction_first_key)
    (gzochid_storage_transaction *, gzochid_storage_store *, size_t *);
  char *(*transaction_next_key)
    (gzochid_storage_transaction *, gzochid_storage_store *, char *, size_t, 
     size_t *);
};

typedef struct _gzochid_storage_engine_interface 
gzochid_storage_engine_interface;

/* An instance of a storage engine that has been loaded as a dynamic module. */

struct _gzochid_storage_engine
{
  GModule *handle; /* The dynamic module handle. */
  gzochid_storage_engine_interface *interface; /* The storage interface. */
};

typedef struct _gzochid_storage_engine gzochid_storage_engine;

/* Define and export the storage engine bootstrap function. This macro is 
   intended for use by the author of a storage engine module. */

#define GZOCHID_STORAGE_INIT_ENGINE(interface)	   \
  G_MODULE_EXPORT gint gzochid_storage_init_engine \
  (gzochid_storage_engine *engine);		   \
  G_MODULE_EXPORT gint gzochid_storage_init_engine \
  (gzochid_storage_engine *engine)		   \
  {						   \
    engine->interface = &(interface);		   \
    return 0;					   \
  }

#endif /* GZOCHID_STORAGE_H */
