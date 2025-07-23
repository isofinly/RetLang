pub(crate) const DEFINES: &str = r#"#define INITIAL_CAPACITY 16
#define MEMORY_SIZE 1024
#define ERR_SUCCESS 0
#define ERR_ALLOC 1
#define ERR_UNDERFLOW 2
#define ERR_TYPE 3
#define ERR_BOUNDS 4
#define ERR_MEMORY 5
"#;

pub(crate) const TYPEDEFS: &str = r#"typedef struct Value {
enum { TYPE_INT, TYPE_STR, TYPE_LABEL } type;
union {
        long long int_val;
        char *str_val;
        void *label_val;
    } u;
} Value;
"#;
