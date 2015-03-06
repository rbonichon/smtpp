#include <caml/mlvalues.h>
#include <caml/callback.h>

void parse(void)
{
     static value *parse_closure = NULL;
     if (parse_closure == NULL) {
          parse_closure = caml_named_value("do_parse");
     }
     caml_callback(*parse_closure, Val_unit);
}
