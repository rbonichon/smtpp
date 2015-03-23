#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/alloc.h>

void parse(char *filename)
{
     static value *parse_closure = NULL;
     if (parse_closure == NULL) {
          parse_closure = caml_named_value("parse_file");
     }
     caml_callback(*parse_closure, caml_copy_string(filename));
}
