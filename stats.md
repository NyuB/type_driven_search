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
|           InMemory|53.| 4.|2736.|1.57652|
|    FileBasedSorted|7266.|172.| 7572.|1.57652|

## 100_k queries benchmark with cutoff at 10

|Index Type \ Op(ms)|        Store|          Get|        Query| Match %|
|-------------------|-------------|-------------|-------------|--------|
|           InMemory|957.|24.|48807.|1.774606|
|    FileBasedSorted|585484.|908.|108976.|1.774606|

## 100_k queries benchmark with cutoff at 100

|Index Type \ Op(ms)|        Store|          Get|        Query| Match %|
|-------------------|-------------|-------------|-------------|--------|
|           InMemory|979.|26.|49454.|1.774606|
|    FileBasedSorted|599245.|1033.|199839.|1.774606|

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

## Filter on return tag for 'get'

|Index Type \ Op(ms)|Store|   Get| Query|Match %|
|-------------------|-----|------|------|-------|
|           InMemory|  46.|    3.| 2535.|1.57652|
|    FileBasedSorted|7516.|  170.| 7309.|1.57652|
|        SqliteBased| 523.|10434.|24889.|1.57652|

## Use '=' instead of 'like'

|Index Type \ Op(ms)| Store|    Get| Query|Match %|
|-------------------|------|-------|------|-------|
|           InMemory|   48.|     4.| 2627.|1.57652|
|    FileBasedSorted| 7312.|   173.| 7429.|1.57652|
|        SqliteBased|  530.|⚡5994.|25075.|1.57652|

## Index join table on tag_id

|Index Type \ Op(ms)|Store|    Get| Query|Match %|
|-------------------|-----|-------|------|-------|
|           InMemory|  50.|     4.| 2552.|1.57652|
|    FileBasedSorted|7225.|   169.| 6975.|1.57652|
|        SqliteBased| 555.|⚡2967.|24970.|1.57652|

## Apply smart (sql) query to (our) query too 

|Index Type \ Op(ms)|Store|  Get|  Query|Match %|
|-------------------|-----|-----|-------|-------|
|           InMemory|  47.|   4.|  2530.|1.57652|
|    FileBasedSorted|7262.| 169.|  7261.|1.57652|
|        SqliteBased| 546.|3059.|⚡3304.|1.57652|

## Limit the query space to functions having 3 matching tags => no impact ...

|Index Type \ Op(ms)|Store|  Get|Query|Match %|
|-------------------|-----|-----|-------------|-------|
|           InMemory|  50.|   4.|2679.|1.57652|
|    FileBasedSorted|7728.| 180.|7552.|1.57652|
|        SqliteBased| 576.|3075.|3549.|1.57652|

## 100_k stores

|Index Type \ Op(ms)|Store|    Get|Query | Match %|
|-------------------|-----|-------|------|--------|
|           InMemory| 930.|    23.|46657.|1.774606|
|        SqliteBased|5342.| 37772.|60179.|1.774606|

## Store and index signatures for optimized get

|Index Type \ Op(ms)|Store|  Get|Query|Match %|
|-------------------|-----|-----|-----|-------|
|           InMemory|  45.|   4.|2558.|1.57652|
|    FileBasedSorted|7390.| 166.|7390.|1.57652|
|        SqliteBased| 575.|⚡31.|3585.|1.57652|
