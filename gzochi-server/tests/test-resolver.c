/* test-resolver.c: Test routines for resolver.c in gzochid.
 * Copyright (C) 2016 Julian Graham
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

#include <glib.h>
#include <glib-object.h>

#include "resolver.h"

#define TEST_TYPE_OBJECT_A test_object_a_get_type ()
GType test_object_a_get_type (void);

struct _TestObjectA
{
  GObject parent_instance;
};

typedef struct _TestObjectA TestObjectA;

struct _TestObjectAClass
{
  GObjectClass parent_class;
};

typedef struct _TestObjectAClass TestObjectAClass;

static inline TestObjectA *
TEST_OBJECT_A (gconstpointer ptr) {
  return G_TYPE_CHECK_INSTANCE_CAST
    (ptr, test_object_a_get_type (), TestObjectA);
}

G_DEFINE_TYPE (TestObjectA, test_object_a, G_TYPE_OBJECT);

#define TEST_TYPE_OBJECT_B test_object_b_get_type ()
GType test_object_b_get_type (void);

struct _TestObjectB
{
  GObject parent_instance;

  TestObjectA *a;
};

typedef struct _TestObjectB TestObjectB;

struct _TestObjectBClass
{
  GObjectClass parent_class;
};

typedef struct _TestObjectBClass TestObjectBClass;

static inline TestObjectB *
TEST_OBJECT_B (gconstpointer ptr) {
  return G_TYPE_CHECK_INSTANCE_CAST
    (ptr, test_object_b_get_type (), TestObjectB);
}

G_DEFINE_TYPE (TestObjectB, test_object_b, G_TYPE_OBJECT);

#define TEST_TYPE_OBJECT_C test_object_c_get_type ()
GType test_object_c_get_type (void);

struct _TestObjectC
{
  GObject parent_instance;

  TestObjectA *a;
  guint value;
};

typedef struct _TestObjectC TestObjectC;

struct _TestObjectCClass
{
  GObjectClass parent_class;
};

typedef struct _TestObjectCClass TestObjectCClass;

static inline TestObjectC *
TEST_OBJECT_C (gconstpointer ptr) {
  return G_TYPE_CHECK_INSTANCE_CAST
    (ptr, test_object_c_get_type (), TestObjectC);
}

G_DEFINE_TYPE (TestObjectC, test_object_c, G_TYPE_OBJECT);

#define TEST_TYPE_OBJECT_D test_object_d_get_type ()
GType test_object_d_get_type (void);

struct _TestObjectD
{
  GObject parent_instance;

  struct _TestObjectD *d;
};

typedef struct _TestObjectD TestObjectD;

struct _TestObjectDClass
{
  GObjectClass parent_class;
};

typedef struct _TestObjectDClass TestObjectDClass;

static inline TestObjectD *
TEST_OBJECT_D (gconstpointer ptr) {
  return G_TYPE_CHECK_INSTANCE_CAST
    (ptr, test_object_d_get_type (), TestObjectD);
}

G_DEFINE_TYPE (TestObjectD, test_object_d, G_TYPE_OBJECT);

#define TEST_TYPE_OBJECT_E test_object_e_get_type ()
GType test_object_e_get_type (void);

struct _TestObjectE
{
  GObject parent_instance;

  GzochidResolutionContext *resolution_context;
};

typedef struct _TestObjectE TestObjectE;

struct _TestObjectEClass
{
  GObjectClass parent_class;
};

typedef struct _TestObjectEClass TestObjectEClass;

static inline TestObjectE *
TEST_OBJECT_E (gconstpointer ptr) {
  return G_TYPE_CHECK_INSTANCE_CAST
    (ptr, test_object_e_get_type (), TestObjectE);
}

G_DEFINE_TYPE (TestObjectE, test_object_e, G_TYPE_OBJECT);

static void
test_object_a_class_init (TestObjectAClass *klass)
{
}

static void
test_object_a_init (TestObjectA *self)
{
}


enum
  {
    TEST_OBJECT_B_PROP_A = 1,
    TEST_OBJECT_B_N_PROPERTIES
  };

static GParamSpec *obj_b_properties[TEST_OBJECT_B_N_PROPERTIES] = { NULL };

static void
test_object_b_dispose (GObject *object)
{
  TestObjectB *self = TEST_OBJECT_B (object);
  g_object_unref (self->a);
}

static void
test_object_b_set_property (GObject *object, guint property_id,
			    const GValue *value, GParamSpec *pspec)
{
  TestObjectB *self = TEST_OBJECT_B (object);

  switch (property_id)
    {
    case TEST_OBJECT_B_PROP_A:
      self->a = g_value_get_object (value);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
      break;
    }
}

static void
test_object_b_class_init (TestObjectBClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  object_class->dispose = test_object_b_dispose;
  object_class->set_property = test_object_b_set_property;

  obj_b_properties[TEST_OBJECT_B_PROP_A] = g_param_spec_object
    ("a", "A", "Set A", TEST_TYPE_OBJECT_A,
     G_PARAM_CONSTRUCT | G_PARAM_WRITABLE);

  g_object_class_install_properties
    (object_class, TEST_OBJECT_B_N_PROPERTIES, obj_b_properties);
}

static void
test_object_b_init (TestObjectB *self)
{
}

enum
  {
    TEST_OBJECT_C_PROP_A = 1,
    TEST_OBJECT_C_PROP_VALUE,
    TEST_OBJECT_C_N_PROPERTIES
  };

static GParamSpec *obj_c_properties[TEST_OBJECT_C_N_PROPERTIES] = { NULL };

static void
test_object_c_dispose (GObject *object)
{
  TestObjectC *self = TEST_OBJECT_C (object);
  g_object_unref (self->a);
}

static void
test_object_c_set_property (GObject *object, guint property_id,
			    const GValue *value, GParamSpec *pspec)
{
  TestObjectC *self = TEST_OBJECT_C (object);

  switch (property_id)
    {
    case TEST_OBJECT_C_PROP_A:
      self->a = g_value_get_object (value);
      break;
    case TEST_OBJECT_C_PROP_VALUE:
      self->value = g_value_get_uint (value);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
      break;
    }
}

static void
test_object_c_class_init (TestObjectCClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  object_class->dispose = test_object_c_dispose;
  object_class->set_property = test_object_c_set_property;

  obj_c_properties[TEST_OBJECT_C_PROP_A] = g_param_spec_object
    ("a", "A", "Set A", TEST_TYPE_OBJECT_A,
     G_PARAM_CONSTRUCT | G_PARAM_WRITABLE);
  obj_c_properties[TEST_OBJECT_C_PROP_VALUE] = g_param_spec_uint
    ("value", "VALUE", "Set value", 0, 0, 0,
     G_PARAM_CONSTRUCT_ONLY | G_PARAM_WRITABLE);

  g_object_class_install_properties
    (object_class, TEST_OBJECT_C_N_PROPERTIES, obj_c_properties);
}

static void
test_object_c_init (TestObjectC *self)
{
}

enum
  {
    TEST_OBJECT_D_PROP_D = 1,
    TEST_OBJECT_D_N_PROPERTIES
  };

static GParamSpec *obj_d_properties[TEST_OBJECT_D_N_PROPERTIES] = { NULL };

static void
test_object_d_dispose (GObject *object)
{
  TestObjectD *self = TEST_OBJECT_D (object);
  g_object_unref (self->d);
}

static void
test_object_d_set_property (GObject *object, guint property_id,
			    const GValue *value, GParamSpec *pspec)
{
  TestObjectD *self = TEST_OBJECT_D (object);

  switch (property_id)
    {
    case TEST_OBJECT_D_PROP_D:
      self->d = g_value_get_object (value);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
      break;
    }
}

static void
test_object_d_class_init (TestObjectDClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  object_class->dispose = test_object_d_dispose;
  object_class->set_property = test_object_d_set_property;

  obj_d_properties[TEST_OBJECT_D_PROP_D] = g_param_spec_object
    ("d", "D", "Set D", TEST_TYPE_OBJECT_D,
     G_PARAM_CONSTRUCT | G_PARAM_WRITABLE);

  g_object_class_install_properties
    (object_class, TEST_OBJECT_D_N_PROPERTIES, obj_d_properties);
}

static void
test_object_d_init (TestObjectD *self)
{
}

enum
  {
    TEST_OBJECT_E_PROP_RESOLUTION_CONTEXT = 1,
    TEST_OBJECT_E_N_PROPERTIES
  };

static GParamSpec *obj_e_properties[TEST_OBJECT_E_N_PROPERTIES] = { NULL };

static void
test_object_e_set_property (GObject *object, guint property_id,
			    const GValue *value, GParamSpec *pspec)
{
  TestObjectE *self = TEST_OBJECT_E (object);

  switch (property_id)
    {
    case TEST_OBJECT_E_PROP_RESOLUTION_CONTEXT:
      self->resolution_context = g_value_get_object (value);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
      break;
    }
}

static void
test_object_e_dispose (GObject *object)
{
  TestObjectE *self = TEST_OBJECT_E (object);

  g_object_unref (self->resolution_context);
}

static void
test_object_e_class_init (TestObjectEClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  object_class->dispose = test_object_e_dispose;
  object_class->set_property = test_object_e_set_property;

  obj_e_properties[TEST_OBJECT_E_PROP_RESOLUTION_CONTEXT] = g_param_spec_object
    ("resolution_context", "RESOLUTION_CONTEXT", "Set the resolution context",
     GZOCHID_TYPE_RESOLUTION_CONTEXT, G_PARAM_CONSTRUCT | G_PARAM_WRITABLE);

  g_object_class_install_properties
    (object_class, TEST_OBJECT_E_N_PROPERTIES, obj_e_properties);
}

static void
test_object_e_init (TestObjectE *self)
{
}

static void
test_require_simple ()
{
  GError *err = NULL;
  TestObjectB *b = gzochid_resolver_require (TEST_TYPE_OBJECT_B, &err);
  
  g_assert_no_error (err);
  g_assert_nonnull (b->a);

  g_object_unref (b);
}

static void
test_require_bad_constructor ()
{
  GError *err = NULL;
  TestObjectC *c = gzochid_resolver_require (TEST_TYPE_OBJECT_C, &err);

  g_assert_null (c);
  g_assert_error
    (err, GZOCHID_RESOLUTION_ERROR,
     GZOCHID_RESOLUTION_ERROR_INCOMPATIBLE_CONSTRUCTOR);

  g_error_free (err);
}

static void
test_require_circular ()
{
  GError *err = NULL;
  TestObjectD *d = gzochid_resolver_require (TEST_TYPE_OBJECT_D, &err);

  g_assert_null (d);
  g_assert_error
    (err, GZOCHID_RESOLUTION_ERROR,
     GZOCHID_RESOLUTION_ERROR_CIRCULAR_DEPENDENCY);

  g_error_free (err);
}

static void
test_require_resolution_context ()
{
  GError *err = NULL;
  TestObjectE *e1 = gzochid_resolver_require (TEST_TYPE_OBJECT_E, &err);
  TestObjectE *e2 = NULL;

  g_assert_nonnull (e1);
  g_assert_no_error (err);

  e2 = gzochid_resolver_require_full
    (e1->resolution_context, TEST_TYPE_OBJECT_E, &err);

  g_assert_nonnull (e2);
  g_assert_no_error (err);

  g_assert (e1 == e2);

  g_object_unref (e1);
  g_object_unref (e2);
}

int
main (int argc, char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  g_test_add_func ("/resolver/require/simple", test_require_simple);
  g_test_add_func
    ("/resolver/require/bad-constructor", test_require_bad_constructor);
  g_test_add_func ("/resolver/require/circular", test_require_circular);
  g_test_add_func
    ("/resolver/require/resolution_context", test_require_resolution_context);

  return g_test_run ();
}
