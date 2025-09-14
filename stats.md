## Operations
- `Store`: stores 10k functions with 10 different types and up to 10 parameters
- `Get`:  performs 1k query for a function signature picked among the stored functions described in `Store` 

## Index Types
- FileBased: a naive implementation storing functions by appending them into a file
- FileBasedSorted: stores the functions in an ordered manner for more efficient queries

## Store uses 'insertion sort', early Get uses 'early return'

|Index Type \ Op(ms)| Store|   Get|
|-------------------|------|------|
|          FileBased|   17.|22402.|
|    FileBasedSorted|83485.|11315.|

## Store uses binary search

|Index Type \ Op(ms)|   Store|   Get|
|-------------------|--------|------|
|          FileBased|     12.|27983.|
|    FileBasedSorted|⚡17031.|16610.|

## Get uses binary search

|Index Type \ Op(ms)| Store|   Get|
|-------------------|------|------|
|          FileBased|    4.|25460.|
|    FileBasedSorted|17615.|⚡119.|

## Store uses C FFI `rename` instead of manual channel-based copy

|Index Type \ Op(ms)|   Store|   Get|
|-------------------|--------|------|
|          FileBased|     7. |26256.|
|    FileBasedSorted|⚡9948. |   92.|

## Introduce query, FileBasedSorted uses an half-ok approach indexing only on return type then searching linearly

|Index Type \ Op(ms)| Store|   Get| Query|
|-------------------|------|------|------|
|          FileBased|    4.|31749.|33633.|
|    FileBasedSorted|11045.|  168.| 6928.|

## Use index indirection instead of storing plain entries sorted

|Index Type \ Op(ms)|  Store|   Get|   Query|
|-------------------|-------|------|--------|
|          FileBased|     5.|35832.|  38154.|
|    FileBasedSorted|⚡6126.|  280.|🐌12695.|

## Also run the benchmark for the ⚡ InMemory version

|Index Type \ Op(ms)|Store|   Get| Query|
|-------------------|-----|------|------|
|           InMemory|  85.|    5.| 3660.|
|          FileBased|   3.|31634.|36173.|
|    FileBasedSorted|6948.|  286.|10547.|

## Store all tags

|Index Type \ Op(ms)|   Store|   Get|   Query|
|-------------------|--------|------|--------|
|           InMemory|     83.|    6.|   3850.|
|          FileBased|      5.|34090.|  36363.|
|    FileBasedSorted|🐌11993.|  306.|🐌47120.|

## Early cutoff at 10

|Index Type \ Op(ms)|        Store|          Get|        Query|Match %|
|-------------------|-------------|-------------|-------------|-------|
|           InMemory|53.1871318817| 4.1389465332|2736.36889458|1.57652|
|    FileBasedSorted|7266.88790321|172.590017319| 7572.5440979|1.57652|

## 100_k queries benchmark with cutoff at 10

|Index Type \ Op(ms)|        Store|          Get|        Query| Match %|
|-------------------|-------------|-------------|-------------|--------|
|           InMemory|957.208156586|24.1668224335|48807.1138859|1.774606|
|    FileBasedSorted|585484.487057|908.435106277|108976.838827|1.774606|

## 100_k queries benchmark with cutoff at 100

|Index Type \ Op(ms)|        Store|          Get|        Query| Match %|
|-------------------|-------------|-------------|-------------|--------|
|           InMemory|979.006052017|26.2060165405|49454.5331001|1.774606|
|    FileBasedSorted|599245.676041|1033.37907791|199839.184999|1.774606|

## Introduce Sqlite, no index just plain functions

|Index Type \ Op(ms)|Store|   Get| Query|Match %|
|-------------------|-----|------|------|-------|
|           InMemory|  47.|    3.| 2574.|1.57652|
|    FileBasedSorted|7739.|  173.| 7635.|1.57652|
|        SqliteBased|😮23.|24171.|24817.|1.57652|

## Store tags and tag -> function
|Index Type \ Op(ms)|        Store|          Get|        Query|Match %|
|-------------------|-------------|-------------|-------------|-------|
|           InMemory|48.|4.|2565.|1.57652|
|    FileBasedSorted|7191.|174.|7311.|1.57652|
|        SqliteBased|🐌528.|23856.|24894.|1.57652|

## Filter on return tag ofr 'get'

|Index Type \ Op(ms)|        Store|          Get|        Query|Match %|
|-------------------|-------------|-------------|-------------|-------|
|           InMemory| 46.532869339|3.94606590271|2535.49408913|1.57652|
|    FileBasedSorted|7516.15691185|170.634031296|7309.22198296|1.57652|
|        SqliteBased|523.818016052|10434.1928959|24889.3740177|1.57652|

## Use '=' instead of 'like'

|Index Type \ Op(ms)|        Store|          Get|        Query|Match %|
|-------------------|-------------|-------------|-------------|-------|
|           InMemory|48.3620166779| 4.7619342804|2627.55823135|1.57652|
|    FileBasedSorted| 7312.9401207|173.263072968|7429.83293533|1.57652|
|        SqliteBased|530.280828476|⚡5994.00210381|25075.8199692|1.57652|

## Index join table on tag_id

|Index Type \ Op(ms)|        Store|          Get|        Query|Match %|
|-------------------|-------------|-------------|-------------|-------|
|           InMemory|50.5759716034|4.16398048401| 2552.9999733|1.57652|
|    FileBasedSorted|7225.08907318|169.780015945|6975.60501099|1.57652|
|        SqliteBased|555.116176605|⚡2967.65899658|24970.7341194|1.57652|
