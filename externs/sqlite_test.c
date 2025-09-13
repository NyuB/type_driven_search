#include "sqlite3/sqlite3.h"
#include <stdio.h>
#include <string.h>

void log_sqlite3_err(const char *context, int err) {
  printf("%s, sqlite error %d:'%s'\n", context, err, sqlite3_errstr(err));
}

int loop_on_all_rows(sqlite3_stmt *Query) {
  int statusCode = SQLITE_ROW;
  while (statusCode == SQLITE_ROW) {
    statusCode = sqlite3_step(Query);
    if (statusCode == SQLITE_ROW) {
      const unsigned char *Result = sqlite3_column_text(Query, 0);
      printf("%s\n", Result);
    } else if (statusCode == SQLITE_DONE) {
      printf("Done querying\n");
    } else {
      log_sqlite3_err("While looping on query", statusCode);
    }
  }
  return statusCode;
}

int main(int argc, char **argv) {
  if (argc < 2) {
    printf("Usage: %s <sqlite db file>\n", argv[0]);
    return 1;
  }

  sqlite3 *Db;
  {
    int err = sqlite3_open(argv[1], &Db);
    if (err != SQLITE_OK) {
      printf("Sqlite error while opening database: %d\n", err);
      return err;
    }
  }

  const char *select_tags = "select name from tags;";
  sqlite3_stmt *Query;
  {
    int err =
        sqlite3_prepare_v2(Db, select_tags, strlen(select_tags), &Query, NULL);
    if (err != SQLITE_OK) {
      printf("Sqlite error while preparing query: %d\n", err);
      return err;
    }
  }

  {

    int err = loop_on_all_rows(Query);
    if (err != SQLITE_DONE) {
      printf("Error while querying: %d\n", err);
      printf("\t'%s\n'", sqlite3_errmsg(Db));
      return err;
    }
  }

  {
    int err = sqlite3_finalize(Query);
    if (err != SQLITE_OK) {
      printf("Sqlite error while finalizing query: %d\n", err);
      return err;
    }
  }

  {
    int err = sqlite3_close(Db);
    if (err != SQLITE_OK) {
      printf("Sqlite error while closing database: %d\n", err);
      return err;
    }
  }
}