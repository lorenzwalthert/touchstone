# can format the benchmark table

    Code
      writeLines(benchmark_table_format(tbl))
    Output
      | Status | Name | Current | Incoming | Interval |
      | :----- | :--: | :------ | :------: | :------: |
      | :rocket: | `b` | 242ms | 100ms | [-60%, -50%] |
      | :exclamation::snail: | `a` | 100ms | 300ms | [+190%, +210%] |
      | :heavy_check_mark: | `c` | 500ms | 500ms | [-1%, +1%] |

