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
