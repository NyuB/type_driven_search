#include "sqlite3/sqlite3.h"

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include <string.h>

/// Opaque type for a sqlite3 connection `sqlite3*`
static struct custom_operations sqlite3_db_connection = {
    "io.github.nyub.sqlite3.db", custom_finalize_default,
    custom_compare_default,      custom_hash_default,
    custom_serialize_default,    custom_deserialize_default,
    custom_compare_ext_default,  custom_fixed_length_default};

static value alloc_db_connection(sqlite3 *db) {
  value caml_result =
      caml_alloc_custom(&sqlite3_db_connection, sizeof(sqlite3 *), 0, 1);
  sqlite3 **custom_alloc = (sqlite3 **)Data_custom_val(caml_result);
  *custom_alloc = db;
  return caml_result;
}

CAMLprim value caml_sqlite3_open(value caml_file) {
  CAMLparam1(caml_file);
  sqlite3 *db;
  sqlite3_open(String_val(caml_file), &db);
  CAMLreturn(alloc_db_connection(db));
}

CAMLprim value caml_sqlite3_close(value caml_sqlite3_db_connection) {
  CAMLparam1(caml_sqlite3_db_connection);
  sqlite3 *db = *(sqlite3 **)Data_custom_val(caml_sqlite3_db_connection);
  sqlite3_close(db);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_sqlite3_exec(value caml_sqlite3_db_connection,
                                 value caml_sql_query,
                                 value caml_sql_callback) {
  CAMLparam2(caml_sqlite3_db_connection, caml_sql_query);
  sqlite3 *db = *(sqlite3 **)Data_custom_val(caml_sqlite3_db_connection);
  const char *sql_query = String_val(caml_sql_query);
  sqlite3_stmt *query;
  {
    int err =
        sqlite3_prepare_v2(db, sql_query, strlen(sql_query), &query, NULL);
    if (err != SQLITE_OK) {
      printf("Error (%d) preparing statement '%s': '%s'\n", err, sql_query,
             sqlite3_errstr(err));
    }
  }

  int statusCode = SQLITE_ROW;
  while (statusCode == SQLITE_ROW) {
    statusCode = sqlite3_step(query);
    if (statusCode == SQLITE_ROW) {
      const unsigned char *result = sqlite3_column_text(query, 0);
      unsigned int result_length = sqlite3_column_bytes(query, 0);
      value caml_result =
          caml_alloc_initialized_string(result_length, (const char *)result);

      caml_callback(caml_sql_callback, caml_result);
    } else if (statusCode != SQLITE_DONE) {
      printf("Error (%d) while looping on query: '%s' ", statusCode,
             sqlite3_errstr(statusCode));
    }
  }

  int err = sqlite3_finalize(query);
  if (err != SQLITE_OK) {
    printf("Error (%d) while finalizing query: '%s'\n", err,
           sqlite3_errstr(err));
    return err;
  }
  CAMLreturn(Val_unit);
}